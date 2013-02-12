import os
import csv

import numpy as np

import config
from expr import Expr, expr_eval
from properties import Process, BreakpointException, TableExpression


class Show(Process):
    def __init__(self, *args):
        Process.__init__(self)
        self.args = args

    def expressions(self):
        for arg in self.args:
            if isinstance(arg, Expr):
                yield arg

    def run(self, context):
        if config.skip_shows:
            print "show skipped",
        else:
            values = [expr_eval(expr, context) for expr in self.args]
            print ' '.join(str(v) for v in values),

    def __str__(self):
        #TODO: the differentiation shouldn't be needed. I guess I should
        # have __repr__ defined for all properties
        str_args = [str(arg) if isinstance(arg, Expr) else repr(arg)
                    for arg in self.args]
        return 'show(%s)' % ', '.join(str_args)


class CSV(Process):
    def __init__(self, *args, **kwargs):
        Process.__init__(self)
        if (len(args) > 1 and
            not any(isinstance(arg, (TableExpression, list, tuple))
                    for arg in args)):
            args = (args,)
        self.args = args
        suffix = kwargs.pop('suffix', '')
        fname = kwargs.pop('fname', None)
        mode = kwargs.pop('mode', 'w')
        if kwargs:
            kwarg, _ = kwargs.popitem()
            raise TypeError("'%s' is an invalid keyword argument for csv()"
                            % kwarg)

        if fname is not None and suffix:
            raise ValueError("csv() can't have both 'suffix' and 'fname' "
                             "arguments")
        if fname is None:
            suffix = "_" + suffix if suffix else ""
            fname = "{entity}_{period}" + suffix + ".csv"
        self.fname = fname
        if mode not in ('w', 'a'):
            raise ValueError("csv() mode argument must be either "
                             "'w' (overwrite) or 'a' (append)")
        self.mode = mode

    def expressions(self):
        for arg in self.args:
            if isinstance(arg, (list, tuple)):
                for expr in arg:
                    if isinstance(expr, Expr):
                        yield expr
            elif isinstance(arg, Expr):
                yield arg

    def run(self, context):
        entity = context['__entity__']
        period = context['period']
        fname = self.fname.format(entity=entity.name, period=period)
        print "writing to", fname, "...",
        file_path = os.path.join(config.output_directory, fname)

        with open(file_path, self.mode + 'b') as f:
            dataWriter = csv.writer(f)
            for arg in self.args:
                if isinstance(arg, TableExpression):
                    data = expr_eval(arg, context)
                elif isinstance(arg, (list, tuple)):
                    data = [[expr_eval(expr, context) for expr in arg]]
                else:
                    data = [[expr_eval(arg, context)]]
                dataWriter.writerows(data)


class RemoveIndividuals(Process):
    def __init__(self, filter):
        Process.__init__(self)
        self.filter = filter

    def expressions(self):
        yield self.filter

    def run(self, context):
        filter_value = expr_eval(self.filter, context)

        if not np.any(filter_value):
            return

        not_removed = ~filter_value

        entity = context['__entity__']
        len_before = len(entity.array)

        #FIXME: this allocates a new (slightly smaller) array. The old
        # array is only discarded when the gc does its job, effectively
        # doubling the peak memory usage for the main array for a while.
        # Seems like another good reason to store columns separately.

        # Shrink array & temporaries. 99% of the function time is spent here.
        entity.array = entity.array[not_removed]
        temp_variables = entity.temp_variables
        for name, temp_value in temp_variables.iteritems():
            if isinstance(temp_value, np.ndarray) and temp_value.shape:
                temp_variables[name] = temp_value[not_removed]

        # update id_to_rownum
        already_removed = entity.id_to_rownum == -1
        already_removed_indices = already_removed.nonzero()[0]
        already_removed_indices_shifted = already_removed_indices - \
                                  np.arange(len(already_removed_indices))

        id_to_rownum = np.arange(len_before)
        id_to_rownum -= filter_value.cumsum()
        #XXX: use np.putmask(id_to_rownum, filter_value, -1)
        id_to_rownum[filter_value] = -1
        entity.id_to_rownum = np.insert(id_to_rownum,
                                        already_removed_indices_shifted,
                                        -1)
        # this version is cleaner and slightly faster but the result is also
        # slightly different: it eliminates ids for dead/removed individuals
        # and this cause bugs in time-related functions
#        ids = entity.array['id']
#        id_to_rownum = np.empty(np.max(ids) + 1, dtype=int)
#        id_to_rownum.fill(-1)
#        id_to_rownum[ids] = np.arange(len(ids), dtype=int)
#        entity.id_to_rownum = id_to_rownum

        print "%d %s(s) removed (%d -> %d)" % (filter_value.sum(), entity.name,
                                               len_before, len(entity.array)),


class Breakpoint(Process):
    def __init__(self, period=None):
        Process.__init__(self)
        self.period = period

    def run(self, context):
        if self.period is None or self.period == context['period']:
            raise BreakpointException()

    def __str__(self):
        if self.period is not None:
            return 'breakpoint(%d)' % self.period
        else:
            return ''

    def expressions(self):
        return ()


class Assert(Process):
    def eval_assertion(self, context):
        raise NotImplementedError()

    def run(self, context):
        if config.assertions == "skip":
            print "assertion skipped",
        else:
            print "assertion",
            failure = self.eval_assertion(context)
            if failure:
                if config.assertions == "warn":
                    print "FAILED:", failure,
                else:
                    raise AssertionError(failure)
            else:
                print "ok",


class AssertTrue(Assert):
    def __init__(self, expr):
        Process.__init__(self)
        self.expr = expr

    def eval_assertion(self, context):
        if not expr_eval(self.expr, context):
            return str(self.expr)

    def expressions(self):
        if isinstance(self.expr, Expr):
            yield self.expr


class AssertEqual(Assert):
    def __init__(self, expr1, expr2):
        Process.__init__(self)
        self.expr1 = expr1
        self.expr2 = expr2

    def eval_assertion(self, context):
        r1 = expr_eval(self.expr1, context)
        r2 = expr_eval(self.expr2, context)
        if isinstance(r1, np.ndarray) and isinstance(r2, np.ndarray):
            passed = np.array_equal(r1, r2)
        else:
            passed = r1 == r2
        if not passed:
            return "%s != %s (%s != %s)" % (r1, r2, self.expr1, self.expr2)

    def expressions(self):
        if isinstance(self.expr1, Expr):
            yield self.expr1
        if isinstance(self.expr2, Expr):
            yield self.expr2


functions = {
    # can't use "print" in python 2.x because it's a keyword, not a function
#    'print': Print,
    'csv': CSV,
    'show': Show,
    'remove': RemoveIndividuals,
    'breakpoint': Breakpoint,
    'assertTrue': AssertTrue,
    'assertEqual': AssertEqual
}
