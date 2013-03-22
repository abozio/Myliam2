from collections import Sequence
from itertools import izip, chain

import numpy as np

from expr import (Expr, Variable,
                  dtype, coerce_types, expr_eval,
                  as_simple_expr, as_string,
                  collect_variables, traverse_expr,
                  get_missing_record, get_missing_vector)
from exprbases import (EvaluableExpression, CompoundExpression,
                       NumexprFunction,
                       FunctionExpression, TableExpression,
                       NumpyCreateArray, NumpyChangeArray)
from context import (EntityContext, context_length, context_subset,
                     new_context_like)
from registry import entity_registry
from utils import PrettyTable


class Min(CompoundExpression):
    def __init__(self, *args):
        CompoundExpression.__init__(self)
        assert len(args) >= 2
        self.args = args

    def build_expr(self):
        expr1, expr2 = self.args[:2]
        expr = Where(expr1 < expr2, expr1, expr2)
        for arg in self.args[2:]:
            expr = Where(expr < arg, expr, arg)

#        Where(Where(expr1 < expr2, expr1, expr2) < expr3,
#              Where(expr1 < expr2, expr1, expr2),
#              expr3)
#        3 where, 3 comparisons = 6 op (or 4 if optimized)
#
#        Where(Where(Where(expr1 < expr2, expr1, expr2) < expr3,
#                    Where(expr1 < expr2, expr1, expr2),
#                    expr3) < expr4,
#              Where(Where(expr1 < expr2, expr1, expr2) < expr3,
#                    Where(expr1 < expr2, expr1, expr2),
#                    expr3),
#              expr4)
#        7 where, 7 comp = 14 op (or 6 if optimized)

        # this version scales better in theory (but in practice, it will depend
        # if numexpr factorize the common subexpression in the above version
        # or not)
#        Where(expr1 < expr2 & expr1 < expr3,
#              expr1,
#              Where(expr2 < expr3, expr2, expr3))
#        2 where, 3 comparisons, 1 and = 6 op
#
#        Where(expr1 < expr2 & expr1 < expr3 & expr1 < expr4,
#              expr1,
#              Where(expr2 < expr3 & expr2 < expr4,
#                    expr2
#                    Where(expr3 < expr4,
#                          expr3,
#                          expr4)))
#        3 where, 6 comp, 3 and = 12 op
        return expr

    def dtype(self, context):
        return coerce_types(context, *self.args)

    def __str__(self):
        return 'min(%s)' % ', '.join(str(arg) for arg in self.args)


class Max(CompoundExpression):
    def __init__(self, *args):
        CompoundExpression.__init__(self)
        assert len(args) >= 2
        self.args = args

    def build_expr(self):
        expr1, expr2 = self.args[:2]
        expr = Where(expr1 > expr2, expr1, expr2)
        for arg in self.args[2:]:
            expr = Where(expr > arg, expr, arg)
        return expr

    def dtype(self, context):
        return coerce_types(context, *self.args)

    def __str__(self):
        return 'max(%s)' % ', '.join(str(arg) for arg in self.args)


class ZeroClip(CompoundExpression):
    def __init__(self, expr1, expr2, expr3):
        CompoundExpression.__init__(self)
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    def build_expr(self):
        return Where((self.expr1 >= self.expr2) & (self.expr1 <= self.expr3),
                     self.expr1,
                     0)

    def dtype(self, context):
        return dtype(self.expr1, context)


# >>> mi = 1
# >>> ma = 10
# >>> a = np.arange(1e7)
#
# >>> timeit np.clip(a, mi, ma)
# 10 loops, best of 3: 127 ms per loop
# >>> timeit np.clip(a, mi, ma, a)
# 10 loops, best of 3: 26.2 ms per loop
# >>> timeit ne.evaluate('where(a < mi, mi, where(a > ma, ma, a))')
# 10 loops, best of 3: 94.1 ms per loop
class Clip(NumpyChangeArray):
    np_func = (np.clip,)
    arg_names = ('a', 'a_min', 'a_max', 'out')


class Sort(NumpyChangeArray):
    np_func = (np.sort,)
    arg_names = ('a', 'axis', 'kind', 'order')

#------------------------------------


class Uniform(NumpyCreateArray):
    np_func = (np.random.uniform,)
    arg_names = ('low', 'high', 'size')


class Normal(NumpyCreateArray):
    np_func = (np.random.normal,)
    arg_names = ('loc', 'scale', 'size')


class RandInt(NumpyCreateArray):
    np_func = (np.random.randint,)
    arg_names = ('low', 'high', 'size')

    def dtype(self, context):
        return int


class Choice(EvaluableExpression):
    func_name = 'choice'

    def __init__(self, choices, weights=None):
        EvaluableExpression.__init__(self)
        if not isinstance(choices, Sequence):
            raise TypeError("choice() first argument should be a sequence "
                            "(tuple or list)")

        if any(isinstance(c, Expr) for c in choices):
            self.choices = choices
        else:
            self.choices = np.array(choices)

        if weights is not None:
            if not isinstance(weights, Sequence):
                raise TypeError("if provided, choice weights should be a "
                                "sequence (tuple or list)")
            if any(isinstance(w, Expr) for w in weights):
                self.bins = weights
            else:
                self.bins = self._weights_to_bins(weights)
        else:
            self.bins = None

    @staticmethod
    def _weights_to_bins(weights):
        bins = np.array([0.0] + list(np.cumsum(weights)))
        error = abs(bins[-1] - 1.0)
        if 0.0 < error <= 1e-6:
            # overshooting a bit is the lesser evil here (the last choice
            # will be picked a tad less than its probability) but we can't
            # easily "correct" that one to 1.0 because in that case, we
            # would have the last bin boundary smaller than the second last
            if str(1.0 - bins[-2]) != str(weights[-1]) and \
               bins[-1] < 1.0:
                print "Warning: last choice probability adjusted to %s " \
                      "instead of %s !" % (1.0 - bins[-2],
                                           weights[-1])
                bins[-1] = 1.0
        return bins

    def evaluate(self, context):
        num = context_length(context)
        choices = self.choices
        if num:
            bins = self.bins
            if bins is None:
                # all values have the same probability
                choices_idx = np.random.randint(len(choices), size=num)
            else:
                if any(isinstance(b, Expr) for b in bins):
                    weights = [expr_eval(expr, context) for expr in bins]
                    bins = self._weights_to_bins(weights)
                u = np.random.uniform(size=num)
                choices_idx = np.digitize(u, bins) - 1
        else:
            choices_idx = []

        if any(isinstance(c, Expr) for c in choices):
            choices = np.array([expr_eval(expr, context) for expr in choices])

        return choices[choices_idx]

    def dtype(self, context):
        return self.choices.dtype

    def traverse(self, context):
        #FIXME: add choices & prob if they are expr
        yield self

    def collect_variables(self, context):
        #FIXME: add choices & prob if they are expr
        return set()

    def __str__(self):
        bins = self.bins
        if bins is None:
            weights_str = ""
        else:
            weights_str = ", %s" % (bins
                                    if any(isinstance(b, Expr) for b in bins)
                                    else '[%s]' % \
                                             ', '.join(str(b)
                                                       for b in np.diff(bins)))
        return "%s(%s%s)" % (self.func_name, list(self.choices), weights_str)


#------------------------------------


class Round(NumpyChangeArray):
    func_name = 'round'  # np.round redirects to np.round_
    np_func = (np.round,)
    arg_names = ('a', 'decimals', 'out')

    def dtype(self, context):
        # result dtype is the same as the input dtype
        res = dtype(self.args[0], context)
        assert res == float
        return res


class Trunc(FunctionExpression):
    func_name = 'trunc'

    def evaluate(self, context):
        return expr_eval(self.expr, context).astype(int)

    def dtype(self, context):
        assert dtype(self.expr, context) == float
        return int

#------------------------------------


class Abs(NumexprFunction):
    func_name = 'abs'

    def dtype(self, context):
        return float


class Log(NumexprFunction):
    func_name = 'log'

    def dtype(self, context):
        return float


class Exp(NumexprFunction):
    func_name = 'exp'

    def dtype(self, context):
        return float


def add_individuals(target_context, children):
    target_entity = target_context['__entity__']
    id_to_rownum = target_entity.id_to_rownum
    array = target_entity.array
    num_rows = len(array)
    num_birth = len(children)
    print "%d new %s(s) (%d -> %d)" % (num_birth, target_entity.name,
                                       num_rows, num_rows + num_birth),

    target_entity.array = np.concatenate((array, children))
    temp_variables = target_entity.temp_variables
    for name, temp_value in temp_variables.iteritems():
        #FIXME: OUCH, this is getting ugly, I'll need a better way to
        # differentiate nd-arrays from "entity" variables
        # I guess having the context contain all entities and a separate
        # globals namespace should fix this problem
        if (isinstance(temp_value, np.ndarray) and
            temp_value.shape == (num_rows,)):
            extra = get_missing_vector(num_birth, temp_value.dtype)
            temp_variables[name] = np.concatenate((temp_value, extra))

    extra_variables = target_context.extra
    for name, temp_value in extra_variables.iteritems():
        if name == '__globals__':
            continue
        if isinstance(temp_value, np.ndarray) and temp_value.shape:
            extra = get_missing_vector(num_birth, temp_value.dtype)
            extra_variables[name] = np.concatenate((temp_value, extra))

    id_to_rownum_tail = np.arange(num_rows, num_rows + num_birth)
    target_entity.id_to_rownum = np.concatenate((id_to_rownum,
                                                 id_to_rownum_tail))


#TODO: inherit from FilteredExpression
#TODO: allow number to be an expression
class CreateIndividual(EvaluableExpression):
    def __init__(self, entity_name=None, filter=None, number=None,
                 num_duplicate=None,return_option=None,expand=None, 
                 numerotation=None, **kwargs):
        self.entity_name = entity_name
        self.filter = filter
        self.kwargs = kwargs
        self.number = number
        #TODO: check num_duplicate exists and is integer
        self.num_duplicate=num_duplicate
        self.expand=expand
        if return_option is not None and return_option is not 'father' :
            raise Exception('the return option for Create Individual is not admitted')
        else: 
            self.return_option = return_option
        
        if numerotation is not None and num_duplicate is None :
            raise Exception('No need to have a numerotation when only one replication')
        else: 
            self.numerotation = numerotation
            
#        if self.num_duplicate is not None and return_option is not 'father' :
#            raise Exception('How can I put the child id if you tell me a father can have many children')

#        assert filter is not None and number is None or \
#               number is not None and filter is None

    def _initial_values(self, array, to_give_birth, num_birth):
        #TODO: use default values for fields which have one
        children = np.empty(num_birth, dtype=array.dtype)
        children[:] = get_missing_record(array)
        return children

    def traverse(self, context):
        for node in traverse_expr(self.filter, context):
            yield node
        for kwarg in self.kwargs.itervalues():
            for node in traverse_expr(kwarg, context):
                yield node
        yield self

    def collect_variables(self, context):
        #FIXME: we need to add variables from self.filter (that's what is
        # needed for the general case -- in expr_eval)
        used_variables = self._collect_kwargs_variables(context)
        return used_variables

    def _collect_kwargs_variables(self, context):
        used_variables = set()
        for v in self.kwargs.itervalues():
            used_variables.update(collect_variables(v, context))
        return used_variables

    def evaluate(self, context):
        source_entity = context['__entity__']
        if self.entity_name is None:
            target_entity = source_entity
        else:
            target_entity = entity_registry[self.entity_name]

        if target_entity is source_entity:
            target_context = context
        else:
            target_context = EntityContext(target_entity,
                                           {'period': context['period']})

        ctx_filter = context.get('__filter__')

        if self.filter is not None and ctx_filter is not None:
            filter_expr = ctx_filter & self.filter
        elif self.filter is not None:
            filter_expr = self.filter
        elif ctx_filter is not None:
            filter_expr = ctx_filter
        else:
            filter_expr = None

        if filter_expr is not None:
            to_give_birth = expr_eval(filter_expr, context)
            num_birth = to_give_birth.sum()
        elif self.number is not None:
            to_give_birth = None
            num_birth = self.number
        else:
            raise Exception('no filter nor number in "new"')

        array = target_entity.array

        id_to_rownum = target_entity.id_to_rownum
        num_individuals = len(id_to_rownum)

        children = self._initial_values(array, to_give_birth, num_birth)
        # select real duplication case
        if self.num_duplicate is not None:
            number_rep = array[self.num_duplicate].compress( array[self.num_duplicate]>0 )
            children = children.repeat(number_rep,axis=0)
            num_birth = number_rep.sum()
            
        if self.expand==True:    
            from numpy.lib.stride_tricks import as_strided
        
            id_add = np.arange(number_rep.max())
            id_add = as_strided(id_add ,
                             shape=number_rep.shape + id_add.shape,
                             strides=(0,) + id_add.strides)
            id_add =  id_add[id_add < number_rep[:, None]]
            one_by_house = array['res'].compress( array[self.num_duplicate]>0 )  
#            indices = np.unique(one_by_house)
#            size_by_id = np.bincount(one_by_house) 
#            size_by_id = size_by_id.compress(size_by_id>0)
#            size_by_id = size_by_id.repeat(size_by_id)  
            id_ini = one_by_house.repeat(number_rep,axis=0)
            decalage = np.zeros(len(one_by_house),dtype=int)  
            indices = np.unique(one_by_house,return_index=True)[1]      
            decalage[indices[1:]] = number_rep[indices]
            decalage = decalage.cumsum().repeat(number_rep,axis=0)
#            decalage = decalage - decalage[0] 
            children['res'] = id_add+decalage+ array['res'].max()+1
            
        remember_id = children['id'].copy()
        
        if num_birth:
            children['id'] = np.arange(num_individuals,
                                       num_individuals + num_birth)
            children['period'] = context['period']

            used_variables = self._collect_kwargs_variables(context)
            child_context = context_subset(context, to_give_birth,
                                           used_variables)
            if to_give_birth is None:
                child_context = new_context_like(context, length=num_birth)
            else:
                child_context = context_subset(context, to_give_birth,
                                               used_variables)
            for k, v in self.kwargs.iteritems():
                children[k] = expr_eval(v, child_context) 
                       
        if self.numerotation is not None:
            from numpy.lib.stride_tricks import as_strided
            initial = np.zeros(len(array), dtype=bool)  
            id_dup = np.arange(number_rep.max())
            id_dup = as_strided(id_dup ,
                             shape=number_rep.shape + id_dup.shape,
                             strides=(0,) + id_dup.strides)
            id_dup =  id_dup[id_dup < number_rep[:, None]]  +1    
            children[self.numerotation] = id_dup

        add_individuals(target_context, children)

        # result is the ids of the new individuals corresponding to the source
        # entity
        # I change here to have the "father" name instead
        if to_give_birth is not None:
            if self.return_option is None:
                result = np.empty(context_length(context), dtype=int)
                result.fill(-1)
                # TODO: must change something to have father size correct with
                # target and not with source.
                if source_entity is target_entity:               
                    extra_bools = np.zeros(num_birth, dtype=bool)
                    to_give_birth = np.concatenate((to_give_birth, extra_bools))
                    
                # Note that np.place is a tad faster, but is currently buggy when
                # working with columns of structured arrays.
                # See http://projects.scipy.org/numpy/ticket/1869
                result[to_give_birth] = children['id']

                return result
            elif self.return_option=='father' :
                father = np.empty(context_length(context), dtype=int)
                father.fill(-1)  
                list_children = np.ones(num_birth, dtype=bool)
                initial = np.zeros(len(array), dtype=bool)
                birth = np.concatenate((initial, list_children))                              
                father[birth] = remember_id
                return father
        else:
            return None
               

    def dtype(self, context):
        return int

class Clone(CreateIndividual):
    def __init__(self, filter=None, num_duplicate=None, return_option=None,
                 numerotation=None,**kwargs):
        CreateIndividual.__init__(self, None, filter, None, 
                                  num_duplicate, return_option,None,
                                  numerotation,**kwargs)
        self.num_duplicate=num_duplicate


    def _initial_values(self, array, to_give_birth, num_birth):
        return array[to_give_birth] 
    
    
class Expand(CreateIndividual):
    def __init__(self, filter=None, num_duplicate=None, return_option=None,
                 numerotation=None,**kwargs):
        CreateIndividual.__init__(self, None, filter, None, 
                                  num_duplicate, return_option,True,
                                  numerotation,**kwargs)
        self.num_duplicate=num_duplicate
   
    def _initial_values(self, array, to_give_birth, num_birth):
        last_id_res = array['res'].max()
        to_return = array[to_give_birth]
        to_return['res'] += last_id_res
        return to_return

class Dump(TableExpression):
    def __init__(self, *args, **kwargs):
        self.expressions = args
        if len(args):
            assert all(isinstance(e, Expr) for e in args), \
                   "dump arguments must be expressions, not a list of them, " \
                   "or strings"

        self.filter = kwargs.pop('filter', None)
        self.missing = kwargs.pop('missing', None)
#        self.periods = kwargs.pop('periods', None)
        self.header = kwargs.pop('header', True)
        if kwargs:
            kwarg, _ = kwargs.popitem()
            raise TypeError("'%s' is an invalid keyword argument for dump()"
                            % kwarg)

    def evaluate(self, context):
        if self.filter is not None:
            filter_value = expr_eval(self.filter, context)
        else:
            filter_value = None

        if self.expressions:
            expressions = list(self.expressions)
        else:
            # extra=False because we don't want globals nor "system" variables
            # (nan, period, __xxx__)
            expressions = [Variable(name)
                           for name in context.keys(extra=False)]

        str_expressions = [str(e) for e in expressions]
        if 'id' not in str_expressions:
            str_expressions.insert(0, 'id')
            expressions.insert(0, Variable('id'))
            id_pos = 0
        else:
            id_pos = str_expressions.index('id')

#        if (self.periods is not None and len(self.periods) and
#            'period' not in str_expressions):
#            str_expressions.insert(0, 'period')
#            expressions.insert(0, Variable('period'))
#            id_pos += 1

        columns = []
        for expr in expressions:
            expr_value = expr_eval(expr, context)
            if (filter_value is not None and isinstance(expr_value, np.ndarray)
                and expr_value.shape):
                expr_value = expr_value[filter_value]
            columns.append(expr_value)

        ids = columns[id_pos]
        if isinstance(ids, np.ndarray) and ids.shape:
            numrows = len(ids)
        else:
            numrows = 1

        # expand scalar columns to full columns in memory
        for idx, col in enumerate(columns):
            dtype = None
            if not isinstance(col, np.ndarray):
                dtype = type(col)
            elif not col.shape:
                dtype = col.dtype.type
            if dtype is not None:
                newcol = np.empty(numrows, dtype=dtype)
                newcol.fill(col)
                columns[idx] = newcol

        data = izip(*columns)
        table = chain([str_expressions], data) if self.header else data
        return PrettyTable(table, self.missing)

    def traverse(self, context):
        for expr in self.expressions:
            for node in traverse_expr(expr, context):
                yield node
        for node in traverse_expr(self.filter, context):
            yield node
        yield self

    def collect_variables(self, context):
        if self.expressions:
            variables = set.union(*[collect_variables(expr, context)
                                    for expr in self.expressions])
        else:
            variables = set(context.keys(extra=False))
        variables |= collect_variables(self.filter, context)
        return variables

    def dtype(self, context):
        return None


#TODO: inherit from NumexprFunction
class Where(Expr):
    def __init__(self, cond, iftrue, iffalse):
        self.cond = cond
        self.iftrue = iftrue
        self.iffalse = iffalse

    def traverse(self, context):
        for node in traverse_expr(self.cond, context):
            yield node
        for node in traverse_expr(self.iftrue, context):
            yield node
        for node in traverse_expr(self.iffalse, context):
            yield node
        yield self

    def as_simple_expr(self, context):
        cond = as_simple_expr(self.cond, context)

        # filter is stored as an unevaluated expression
        filter_expr = context.get('__filter__')

        if filter_expr is None:
            context['__filter__'] = self.cond
        else:
            context['__filter__'] = filter_expr & self.cond
        iftrue = as_simple_expr(self.iftrue, context)

        if filter_expr is None:
            context['__filter__'] = ~self.cond
        else:
            context['__filter__'] = filter_expr & ~self.cond
        iffalse = as_simple_expr(self.iffalse, context)

        context['__filter__'] = None
        return Where(cond, iftrue, iffalse)

    def as_string(self):
        return "where(%s, %s, %s)" % (as_string(self.cond),
                                      as_string(self.iftrue),
                                      as_string(self.iffalse))

    def __str__(self):
        return "if(%s, %s, %s)" % (self.cond, self.iftrue, self.iffalse)
    __repr__ = __str__

    def dtype(self, context):
        assert dtype(self.cond, context) == bool
        return coerce_types(context, self.iftrue, self.iffalse)

    def collect_variables(self, context):
        condvars = collect_variables(self.cond, context)
        iftruevars = collect_variables(self.iftrue, context)
        iffalsevars = collect_variables(self.iffalse, context)
        return condvars | iftruevars | iffalsevars


functions = {
    # random
    'uniform': Uniform,
    'normal': Normal,
    'choice': Choice,
    'randint': RandInt,
    # per element
    'min': Min,
    'max': Max,
    'abs': Abs,
    'clip': Clip,
    'zeroclip': ZeroClip,
    'round': Round,
    'trunc': Trunc,
    'exp': Exp,
    'log': Log,
    'where': Where,
    # misc
    'sort': Sort,
    'new': CreateIndividual,
    'clone': Clone,
    'expand': Expand,
    'dump': Dump,
}
