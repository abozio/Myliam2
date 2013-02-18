def align_get_indices_nd(context, filter_value, score,
                         expressions, possible_values, probabilities,
                         take_filter=None, leave_filter=None, weights=None,
                         past_error=None):
    assert len(expressions) == len(possible_values)
  
    if filter_value is not None:
        num_to_align = np.sum(filter_value)
    else:
        num_to_align = context_length(context)

    #TODO: allow any temporal variable
    if 'period' in [str(e) for e in expressions]:
        period = context['period']
        expressions, possible_values, probabilities = \
            extract_period(period, expressions, possible_values,
                           probabilities)

    if expressions:
        assert len(probabilities) == prod(len(pv) for pv in possible_values)

        # retrieve the columns we need to work with
        columns = [expr_eval(expr, context) for expr in expressions]
        if filter_value is not None:
            groups = partition_nd(columns, filter_value, possible_values)
        else:
            groups = partition_nd(columns, True, possible_values)
    else:
        if filter_value is not None:
            groups = [filter_value.nonzero()[0]]
        else:
            groups = [np.arange(num_to_align)]
        assert len(probabilities) == 1

    # the sum is not necessarily equal to len(a), because some individuals
    # might not fit in any group (eg if some alignment data is missing)
    num_aligned = sum(len(g) for g in groups)
    if num_aligned < num_to_align:
        if filter_value is not None:
            to_align = set(filter_value.nonzero()[0])
        else:
            to_align = set(xrange(num_to_align))
        aligned = set()
        for member_indices in groups:
            aligned |= set(member_indices)
        unaligned = to_align - aligned
        print("Warning: %d individual(s) do not fit in any alignment category"
              % len(unaligned))
        print(PrettyTable([['id'] + expressions] +
                          [[col[row] for col in [context['id']] + columns]
                           for row in unaligned]))

    if filter_value is not None:
        bool_filter_value = filter_value.copy()
    else:
        bool_filter_value = True

    maybe_filter = bool_filter_value
    if take_filter is not None:
        #XXX: I wonder if users would prefer if filter_value was taken into
        # account or not. This only impacts what it displayed on the console,
        # but still...
        take = np.sum(take_filter)
        take_indices = (take_filter & bool_filter_value).nonzero()[0]
        maybe_filter &= ~take_filter
    else:
        take = 0
        take_indices = None

    if leave_filter is not None:
        leave = np.sum(leave_filter)
        maybe_filter &= ~leave_filter
    else:
        leave = 0

    if take_filter is not None or leave_filter is not None:
        maybe_indices = maybe_filter.nonzero()[0]
    else:
        maybe_indices = None

    total_underflow = 0
    total_overflow = 0
    total_affected = 0
    total_indices = []
    to_split_indices = []
    to_split_overflow = []
    for group_idx, members_indices, probability in izip(count(), groups,
                                                        probabilities):
        if len(members_indices):
            if weights is None:
                expected = len(members_indices) * probability
            else:
                expected = np.sum(weights[members_indices]) * probability
            affected = int(expected)
            if past_error is not None:
                group_overflow = past_error[group_idx]
                if group_overflow != 0:
                    affected -= group_overflow
                past_error[group_idx] = 0

            if random.random() < expected - affected:
                affected += 1
            total_affected += affected

            if take_indices is not None:
                group_always = np.intersect1d(members_indices, take_indices,
                                              assume_unique=True)
                if weights is None:
                    num_always = len(group_always)
                else:
                    num_always = np.sum(weights[group_always])
                total_indices.extend(group_always)
            else:
                num_always = 0

            if affected > num_always:
                if maybe_indices is not None:
                    group_maybe_indices = np.intersect1d(members_indices,
                                                         maybe_indices,
                                                         assume_unique=True)
                else:
                    group_maybe_indices = members_indices
                if isinstance(score, np.ndarray):
                    maybe_members_rank_value = score[group_maybe_indices]
                    sorted_local_indices = np.argsort(maybe_members_rank_value)
                    sorted_global_indices = \
                        group_maybe_indices[sorted_local_indices]
                else:
                    assert isinstance(score, (bool, int, float))
                    # if the score expression is a constant, we don't need to
                    # sort indices. In that case, the alignment will take
                    # the last individuals created first (highest id).
                    sorted_global_indices = group_maybe_indices

                # maybe_to_take is always > 0
                maybe_to_take = affected - num_always
                if weights is None:
                    # take the last X individuals (ie those with the highest
                    # score)
                    indices_to_take = sorted_global_indices[-maybe_to_take:]
                else:
                    maybe_weights = weights[sorted_global_indices]

                    # we need to invert the order because members are sorted
                    # on score ascendingly and we need to take those with
                    # highest score.
                    weight_sums = np.cumsum(maybe_weights[::-1])

                    threshold_idx = np.searchsorted(weight_sums, maybe_to_take)
                    if threshold_idx < len(weight_sums):
                        num_to_take = threshold_idx + 1
                        # if there is enough weight to reach "maybe_to_take"
                        overflow = weight_sums[threshold_idx] - maybe_to_take
                        if overflow > 0:
                            # the next individual has too much weight, so we
                            # need to split it.
                            id_to_split = sorted_global_indices[threshold_idx]
                            past_error[group_idx] = overflow
                            to_split_indices.append(id_to_split)
                            to_split_overflow.append(overflow)
                        else:
                            # we got exactly the number we wanted
                            assert overflow == 0
                    else:
                        # we can't reach our target number of individuals
                        # (probably because of a "leave" filter), so we
                        # take all the ones we have
                        #XXX: should we add *this* underflow to the past_error
                        # too? It would probably accumulate!
                        num_to_take = len(weight_sums)
                    indices_to_take = sorted_global_indices[-num_to_take:]

                underflow = maybe_to_take - len(indices_to_take)
                if underflow > 0:
                    total_underflow += underflow
                total_indices.extend(indices_to_take)
            elif affected < num_always:
                total_overflow += num_always - affected
# this assertion is only valid in the non weighted case
#    assert len(total_indices) == \
#           total_affected + total_overflow - total_underflow
    print(" %d/%d" % (len(total_indices), num_aligned), end=" ")
    if (take_filter is not None) or (leave_filter is not None):
        print("[take %d, leave %d]" % (take, leave), end=" ")
    if total_underflow:
        print("UNDERFLOW: %d" % total_underflow, end=" ")
    if total_overflow:
        print("OVERFLOW: %d" % total_overflow, end=" ")

    if to_split_indices:
        return total_indices, (to_split_indices, to_split_overflow)
    else:
        return total_indices, None

class Alignment(FilteredExpression):
    func_name = 'align'

    def __init__(self, score_expr, filter=None, take=None, leave=None,
                 fname=None,
                 expressions=None, possible_values=None, probabilities=None,
                 on_overflow='default'):
        super(Alignment, self).__init__(score_expr, filter)

        assert ((expressions is not None and
                 possible_values is not None and
                 probabilities is not None) or
                (fname is not None))

        if fname is not None:
            self.load(fname)
        else:
            self.expressions = [Variable(e) if isinstance(e, basestring) else e
                                for e in expressions]
            self.possible_values = possible_values
            self.probabilities = probabilities

        self.take_filter = take
        self.leave_filter = leave
        self.on_overflow = on_overflow
        self.overflows = None

    def traverse(self, context):
        for node in FilteredExpression.traverse(self, context):
            yield node
        for expr in self.expressions:
            for node in traverse_expr(expr, context):
                yield node
        for node in traverse_expr(self.take_filter, context):
            yield node
        for node in traverse_expr(self.leave_filter, context):
            yield node
        yield self

    def collect_variables(self, context):
        variables = FilteredExpression.collect_variables(self, context)
        if self.expressions:
            variables |= set.union(*[collect_variables(expr, context)
                                     for expr in self.expressions])
        variables |= collect_variables(self.take_filter, context)
        variables |= collect_variables(self.leave_filter, context)
        return variables

    def load(self, fpath):
        from exprparser import parse

        with open(os.path.join(config.input_directory, fpath), "rb") as f:
            reader = csv.reader(f)
            lines = skip_comment_cells(strip_rows(reader))
            header = lines.next()
            self.expressions = [parse(s, autovariables=True) for s in header]
            table = []
            for line in lines:
                if any(value == '' for value in line):
                    raise Exception("empty cell found in %s" % fpath)
                table.append([eval(value) for value in line])
        ndim = len(header)
        unique_last_d, dupe_last_d = unique_duplicate(table.pop(0))
        if dupe_last_d:
            print("Duplicate column header value(s) (for '%s') in '%s': %s"
                  % (header[-1], fpath,
                     ", ".join(str(v) for v in dupe_last_d)))
            raise Exception("bad alignment data in '%s': found %d "
                            "duplicate column header value(s)"
                            % (fpath, len(dupe_last_d)))

        # strip the ndim-1 first columns
        headers = [[line.pop(0) for line in table]
                   for _ in range(ndim - 1)]

        possible_values = [list(unique(values)) for values in headers]
        if ndim > 1:
            # having duplicate values is normal when there are more than 2
            # dimensions but we need to test whether there are duplicates of
            # combinations.
            dupe_combos = list(duplicates(zip(*headers)))
            if dupe_combos:
                print("Duplicate row header value(s) in '%s':" % fpath)
                print(PrettyTable(dupe_combos))
                raise Exception("bad alignment data in '%s': found %d "
                                "duplicate row header value(s)"
                                % (fpath, len(dupe_combos)))

        possible_values.append(unique_last_d)
        self.possible_values = possible_values
        self.probabilities = list(chain.from_iterable(table))
        num_possible_values = prod(len(values) for values in possible_values)
        if len(self.probabilities) != num_possible_values:
            raise Exception("incoherent alignment data in '%s': %d data cells "
                            "found while it should be %d based on the number "
                            "of possible values in headers (%s)"
                            % (fpath,
                               len(self.probabilities),
                               num_possible_values,
                               ' * '.join(str(len(values))
                                          for values in possible_values)))

    def evaluate(self, context):
        scores = expr_eval(self.expr, context)

        on_overflow = self.on_overflow
        if on_overflow == 'default':
            on_overflow = context.get('__on_align_overflow__', 'carry')

        #XXX: I should try to pre-parse weight_col in the entity, rather than
        # here, possibly allowing expressions. Not sure it has any use, but it
        # should not cost us much
        weight_col = context.get('__weight_col__')
        if weight_col is not None:
            weights = expr_eval(Variable(weight_col), context)
            if on_overflow == 'carry' and self.overflows is None:
                self.overflows = np.zeros(len(self.probabilities))
        else:
            weights = None

        ctx_filter = context.get('__filter__')
        if self.filter is not None:
            if ctx_filter is not None:
                filter_expr = ctx_filter & self.filter
            else:
                filter_expr = self.filter
        else:
            if ctx_filter is not None:
                filter_expr = ctx_filter
            else:
                filter_expr = None

        if filter_expr is not None:
            filter_value = expr_eval(filter_expr, context)
        else:
            filter_value = None

        take_filter = expr_eval(self.take_filter, context) \
                      if self.take_filter is not None else None
        leave_filter = expr_eval(self.leave_filter, context) \
                       if self.leave_filter is not None \
                       else None

        indices, overflows = \
            align_get_indices_nd(context, filter_value, scores,
                                 self.expressions, self.possible_values,
                                 self.probabilities,
                                 take_filter, leave_filter, weights,
                                 self.overflows)

        if overflows is not None:
            to_split_indices, to_split_overflow = overflows
            if on_overflow == 'split':
                num_birth = len(to_split_indices)
                source_entity = context['__entity__']
                target_entity = source_entity
                array = target_entity.array
                clones = array[to_split_indices]
                id_to_rownum = target_entity.id_to_rownum
                num_individuals = len(id_to_rownum)
                clones['id'] = np.arange(num_individuals,
                                         num_individuals + num_birth)
                #FIXME: self.weight_col is not defined
                clones[self.weight_col] = to_split_overflow
                array[self.weight_col][to_split_indices] -= to_split_overflow
                add_individuals(context, clones)

        return {'values': True, 'indices': indices}

    def dtype(self, context):
        return bool

