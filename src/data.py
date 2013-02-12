import time


import numpy as np

from expr import normalize_type, get_missing_value, get_missing_record
from utils import loop_wh_progress, time2str, safe_put
import tables

def table_size(table):
    return (len(table) * table.dtype.itemsize) / 1024.0 / 1024.0


def get_fields(array):
    dtype = array.dtype
    field_types = dtype.fields
    return [(name, normalize_type(field_types[name][0].type))
            for name in dtype.names]


def assertValidFields(array, wanted_fields, allowed_missing=None):
    # extract types from field description and normalise to python types
    actual_fields = get_fields(array)

    # check that all required fields are present
    wanted_names = set(name for name, _ in wanted_fields)
    actual_names = set(name for name, _ in actual_fields)
    allowed_missing = set(allowed_missing) if allowed_missing is not None \
                                           else set()
    missing = wanted_names - actual_names - allowed_missing
    if missing:
        raise Exception("Missing field(s) in hdf5 input file: %s"
                        % ', '.join(missing))

    # check that types match
    common_fields1 = sorted((name, type_) for name, type_ in actual_fields
                            if name in wanted_names)
    common_fields2 = sorted((name, type_) for name, type_ in wanted_fields
                            if name in actual_names)
    bad_fields = []
    for (name1, t1), (name2, t2) in zip(common_fields1, common_fields2):
        assert name1 == name2, "%s != %s" % (name1, name2)
        if t1 != t2:
            bad_fields.append((name1, t2.__name__, t1.__name__))
    if bad_fields:
        bad_fields_str = "\n".join(" - %s: %s instead of %s" % f
                                   for f in bad_fields)
        raise Exception("Field types in hdf5 input file differ from those "
                        "defined in the simulation:\n%s" % bad_fields_str)


def add_and_drop_fields(array, output_fields, output_array=None):
    output_dtype = np.dtype(output_fields)
    output_names = set(output_dtype.names)
    input_names = set(array.dtype.names)
    common_fields = output_names & input_names
    missing_fields = output_names - input_names
    if output_array is None:
        output_array = np.empty(len(array), dtype=output_dtype)
        for fname in missing_fields:
            output_array[fname] = get_missing_value(output_array[fname])
    else:
        assert output_array.dtype == output_dtype
    for fname in common_fields:
        output_array[fname] = array[fname]
    return output_array


def mergeSubsetInArray(output, id_to_rownum, subset, first=False):
    if subset.dtype == output.dtype and len(subset) == len(output):
        return subset
    elif subset.dtype == output.dtype:
        safe_put(output, id_to_rownum[subset['id']], subset)
        return output

    output_names = output.dtype.names
    subset_names = subset.dtype.names
    names_to_copy = set(subset_names) & set(output_names)
    if len(subset) == len(output):
        for fname in names_to_copy:
            output[fname] = subset[fname]
        return output
    else:
        rownums = id_to_rownum[subset['id']]
        #TODO: this is a gross approximation, more research is needed to get
        # a better threshold. It might also depend on "first".
        if len(names_to_copy) > len(output_names) / 2:
            if first:
                subset_all_cols = np.empty(len(subset), dtype=output.dtype)
                for fname in set(output_names) - set(subset_names):
                    subset_all_cols[fname] = \
                        get_missing_value(subset_all_cols[fname])
            else:
                subset_all_cols = output[rownums]
                # Note that all rows which correspond to rownums == -1 have
                # wrong values (they have the value of the last row) but it is
                # not necessary to correct them since they will not be copied
                # back into output_array.
                # np.putmask(subset_all_cols, rownums == -1, missing_row)
            for fname in names_to_copy:
                subset_all_cols[fname] = subset[fname]
            safe_put(output, rownums, subset_all_cols)
        else:
            for fname in names_to_copy:
                safe_put(output[fname], rownums, subset[fname])
        return output


def mergeArrays(array1, array2, result_fields='union'):
    fields1 = get_fields(array1)
    fields2 = get_fields(array2)
    #TODO: check that common fields have the same type
    if result_fields == 'union':
        names1 = set(array1.dtype.names)
        fields_notin1 = [(name, type_) for name, type_ in fields2
                         if name not in names1]
        output_fields = fields1 + fields_notin1
    elif result_fields == 'array1':
        output_fields = fields1
    else:
        raise ValueError('%s in not a valid value for result_fields argument' %
                         result_fields)

    output_dtype = np.dtype(output_fields)

    ids1 = array1['id']
    ids2 = array2['id']
    all_ids = np.union1d(ids1, ids2)
    max_id = all_ids[-1]

    # compute new id_to_rownum
    id_to_rownum = np.empty(max_id + 1, dtype=int)
    id_to_rownum.fill(-1)
    for rownum, rowid in enumerate(all_ids):
        id_to_rownum[rowid] = rownum

    # 1) create resulting array
    ids1_complete = len(ids1) == len(all_ids)
    ids2_complete = len(ids2) == len(all_ids)
    output_is_arr1 = array1.dtype == output_dtype and ids1_complete
    output_is_arr2 = array2.dtype == output_dtype and ids2_complete
    arr1_complete = set(fields1) >= set(output_fields) and ids1_complete
    arr2_complete = set(fields2) >= set(output_fields) and ids2_complete
    if output_is_arr2:
        output_array = array2
    elif output_is_arr1:
        output_array = array1
    elif arr1_complete or arr2_complete:
        output_array = np.empty(len(all_ids), dtype=output_dtype)
    else:
        output_array = np.empty(len(all_ids), dtype=output_dtype)
        output_array[:] = get_missing_record(output_array)

    # 2) copy data from array1
    if not arr2_complete:
        output_array = mergeSubsetInArray(output_array, id_to_rownum,
                                          array1, first=True)

    # 3) copy data from array2
    if not output_is_arr2:
        output_array = mergeSubsetInArray(output_array, id_to_rownum, array2)

    return output_array, id_to_rownum


def appendTable(input_table, output_table, chunksize=10000, condition=None,
                stop=None, show_progress=False):
    if input_table.dtype != output_table.dtype:
        output_fields = get_fields(output_table)
    else:
        output_fields = None

    if stop is None:
        numrows = len(input_table)
    else:
        numrows = stop

    if not chunksize:
        chunksize = numrows

    num_chunks, remainder = divmod(numrows, chunksize)
    if remainder > 0:
        num_chunks += 1

    if output_fields is not None:
        expanded_data = np.empty(chunksize, dtype=np.dtype(output_fields))
        expanded_data[:] = get_missing_record(expanded_data)

    def copyChunk(chunk_idx, chunk_num):
        start = chunk_num * chunksize
        stop = min(start + chunksize, numrows)
        if condition is not None:
            input_data = input_table.readWhere(condition,
                                               start=start, stop=stop)
        else:
            input_data = input_table.read(start, stop)

        if output_fields is not None:
            # use our pre-allocated buffer (except for the last chunk)
            if len(input_data) == len(expanded_data):
                output_data = add_and_drop_fields(input_data, output_fields,
                                                  expanded_data)
            else:
                output_data = add_and_drop_fields(input_data, output_fields)
        else:
            output_data = input_data

        output_table.append(output_data)
        output_table.flush()

    if show_progress:
        loop_wh_progress(copyChunk, range(num_chunks))
    else:
        for chunk in range(num_chunks):
            copyChunk(chunk, chunk)

    return output_table


def copyTable(input_table, output_file, output_node, output_fields=None,
              chunksize=10000, condition=None, stop=None, show_progress=False,
              **kwargs):
    complete_kwargs = {'title': input_table._v_title,
                      }
#                       'filters': input_table.filters}
    complete_kwargs.update(kwargs)
    if output_fields is None:
        output_dtype = input_table.dtype
    else:
        output_dtype = np.dtype(output_fields)
    output_table = output_file.createTable(output_node, input_table.name,
                                           output_dtype, **complete_kwargs)
    return appendTable(input_table, output_table, chunksize, condition,
                       stop=stop, show_progress=show_progress)


#XXX: should I make a generic n-way array merge out of this?
# this is a special case though because:
# 1) all arrays have the same columns
# 2) we have id_to_rownum already computed for each array
def buildArrayForPeriod(input_table, output_fields, input_rows, input_index,
                        start_period):
    periods_before = [p for p in input_rows.iterkeys() if p <= start_period]
    if not periods_before:
        id_to_rownum = np.empty(0, dtype=int)
        output_array = np.empty(0, np.dtype(output_fields))
        return output_array, id_to_rownum

    periods_before.sort()
    # take the last period which we have data for
    target_period = periods_before[-1]

    # computing is present
    max_id = len(input_index[target_period]) - 1
    is_present = np.zeros(max_id + 1, dtype=bool)
    for period in periods_before:
        period_id_to_rownum = input_index[period]
        present_in_period = period_id_to_rownum != -1
        present_in_period.resize(max_id + 1)
        is_present |= present_in_period

    # if all individuals are present in the target period, we are done already!
    if np.array_equal(present_in_period, is_present):
        start, stop = input_rows[target_period]
        input_array = input_table.read(start=start, stop=stop)
        return (add_and_drop_fields(input_array, output_fields),
                period_id_to_rownum)

    # building id_to_rownum for the target period
    id_to_rownum = np.empty(max_id + 1, dtype=int)
    id_to_rownum.fill(-1)
    rownum = 0
    for row_id, present in enumerate(is_present):
        if present:
            id_to_rownum[row_id] = rownum
            rownum += 1

    # computing the source row for each destination row
    # we loop over the periods before start_period in reverse order
    output_array_source_rows = np.empty(rownum, dtype=int)
    output_array_source_rows.fill(-1)
    for period in periods_before[::-1]:
        start, stop = input_rows[period]
        input_rownums = np.arange(start, stop)

        input_id_to_rownum = input_index[period]
        id_is_in_period = input_id_to_rownum != -1

        # which output rows are filled by input for this period
        output_rownums = id_to_rownum[id_is_in_period]

        # get source rows (in the global array) for individuals in this period
        source_rows = output_array_source_rows[output_rownums]

        # if their source row is already known, leave them alone
        need_update = source_rows == -1

        # global indices of rows which are not set yet (for this period)
        rows_to_update = output_rownums[need_update]

        # source row for those rows
        local_source_rows = input_rownums[need_update]

        # update the source row for those rows
        safe_put(output_array_source_rows, rows_to_update, local_source_rows)

        if np.all(output_array_source_rows != -1):
            break

    # reading data
    output_array = input_table.readCoordinates(output_array_source_rows)
    output_array = add_and_drop_fields(output_array, output_fields)

    return output_array, id_to_rownum


def index_table(table):
    '''
    table is an iterable of rows, each row is a mapping (name -> value).
    Rows must contain at least 'period' and 'id' columns and must be sorted
    by period.
    '''
    rows_per_period = {}
    id_to_rownum_per_period = {}
    temp_id_to_rownum = []
    max_id_so_far = -1
    current_period = None
    start_row = None
    for idx, row in enumerate(table):
        period, row_id = row['period'], row['id']
        if period != current_period:
            # 0 > None is True
            if period < current_period:
                raise Exception("data is not time-ordered")
            if start_row is not None:
                rows_per_period[current_period] = start_row, idx
                # assumes the data is sorted on period then id
                id_to_rownum = np.array(temp_id_to_rownum)
                id_to_rownum_per_period[current_period] = id_to_rownum
                temp_id_to_rownum = [-1] * (max_id_so_far + 1)
            start_row = idx
            current_period = period
        if row_id > max_id_so_far:
            extra = [-1] * (row_id - max_id_so_far)
            temp_id_to_rownum.extend(extra)
        temp_id_to_rownum[row_id] = idx - start_row
        max_id_so_far = max(max_id_so_far, row_id)
    if current_period is not None:
        rows_per_period[current_period] = (start_row, len(table))
        id_to_rownum_per_period[current_period] = np.array(temp_id_to_rownum)
    return rows_per_period, id_to_rownum_per_period


def index_table_light(table):
    '''
    table is an iterable of rows, each row is a mapping (name -> value)
    Rows must contain at least a 'period' column and must be sorted by period.
    '''
    rows_per_period = {}
    current_period = None
    start_row = None
    for idx, row in enumerate(table):
        period = row['period']
        if period != current_period:
            # 0 > None is True
            if period < current_period:
                raise Exception("data is not time-ordered")
            if start_row is not None:
                rows_per_period[current_period] = (start_row, idx)
            start_row = idx
            current_period = period
    if current_period is not None:
        rows_per_period[current_period] = (start_row, len(table))
    return rows_per_period


class IndexedTable(object):
    def __init__(self, table, period_index, id2rownum_per_period):
        self.table = table
        self.period_index = period_index
        self.id2rownum_per_period = id2rownum_per_period

    # In the future, we will probably want a more flexible interface, but
    # it will need a lot more machinery (query language and so on) so let's
    # stick with something simple for now.
    def read(self, period, ids=None, field=None):
        if period not in self.period_index:
            raise Exception('no data for period %d' % period)
        if ids is None:
            start, stop = self.period_index[period]
            return self.table.read(start=start, stop=stop, field=field)
        else:
            raise NotImplementedError('reading only some ids is not '
                                      'implemented yet')

    @property
    def base_period(self):
        return min(self.period_index.keys())


class DataSet(object):
    pass


def index_tables(globals_fields, entities, fpath):
    print "reading data from %s ..." % fpath

    input_file = tables.openFile(fpath, mode="r")
    try:
        periodic_globals = None
        input_root = input_file.root

        if globals_fields:
            if ('globals' not in input_root or 
                'periodic' not in input_root.globals):
                raise Exception('could not find globals in the input data '
                                'file (but they are declared in the '
                                'simulation file)')
            globals_table = input_root.globals.periodic
            # load globals in memory
            #FIXME: make sure either period or PERIOD is present
            assertValidFields(globals_table, globals_fields,
                              allowed_missing=('period', 'PERIOD'))
            periodic_globals = globals_table.read()

        input_entities = input_root.entities

        entities_tables = {}
        dataset = {'globals': periodic_globals,
                   'entities': entities_tables}

        print " * indexing tables"
        for ent_name, entity in entities.iteritems():
            print "    -", ent_name, "...",

            table = getattr(input_entities, ent_name)
            assertValidFields(table, entity.fields, entity.missing_fields)

            start_time = time.time()
            rows_per_period, id_to_rownum_per_period = index_table(table)
            indexed_table = IndexedTable(table, rows_per_period,
                                         id_to_rownum_per_period)
            entities_tables[ent_name] = indexed_table
            print "done (%s elapsed)." % time2str(time.time() - start_time)
    except:
        input_file.close()
        raise

    return input_file, dataset


class DataSource(object):
    pass


# A data source is not necessarily read-only, but should be connected to
# only one file, so in our case we should have one instance for input and the
# other (used both for read and write) for the output.
class H5Data(DataSource):
    def __init__(self, input_path, output_path):
        self.input_path = input_path
        self.output_path = output_path

    def load(self, globals_fields, entities):
        h5file, dataset = index_tables(globals_fields, entities,
                                       self.output_path)
        entities_tables = dataset['entities']
        for ent_name, entity in entities.iteritems():
# this is what should happen
#            entity.indexed_input_table = entities_tables[ent_name]
#            entity.indexed_output_table = entities_tables[ent_name]
            table = entities_tables[ent_name]

            entity.input_index = table.id2rownum_per_period
            entity.input_rows = table.period_index
            entity.input_table = table.table

            entity.output_index = table.id2rownum_per_period
            entity.output_rows = table.period_index
            entity.table = table.table

            entity.base_period = table.base_period

        return h5file, None, dataset['globals']

    def run(self, globals_fields, entities, start_period):
        input_file, dataset = index_tables(globals_fields, entities,
                                           self.input_path)
        output_file = tables.openFile(self.output_path, mode="w")

        try:
            if dataset['globals'] is not None:
                output_globals = output_file.createGroup("/", "globals",
                                                         "Globals")
                copyTable(input_file.root.globals.periodic, output_file,
                          output_globals)

            entities_tables = dataset['entities']
            output_entities = output_file.createGroup("/", "entities",
                                                      "Entities")
            print " * copying tables"
            for ent_name, entity in entities.iteritems():
                print ent_name, "..."

                # main table

                table = entities_tables[ent_name]

                entity.input_index = table.id2rownum_per_period
                entity.input_rows = table.period_index
                entity.input_table = table.table
                entity.base_period = table.base_period

# this is what should happen
#                entity.indexed_input_table = entities_tables[ent_name]
#                entity.indexed_output_table = entities_tables[ent_name]

                #TODO: copying the table and generally preparing the output
                # file should be a different method than indexing
                print " * copying table..."
                start_time = time.time()
                input_rows = entity.input_rows
                output_rows = dict((p, rows)
                                   for p, rows in input_rows.iteritems()
                                   if p < start_period)
                if output_rows:
                    # stoprow = last row of the last period before start_period
                    _, stoprow = input_rows[max(output_rows.iterkeys())]
                else:
                    stoprow = 0

                output_table = copyTable(table.table,
                                         output_file, output_entities,
                                         entity.fields, stop=stoprow,
                                         show_progress=True)
                entity.output_rows = output_rows
                print "done (%s elapsed)." % time2str(time.time() - start_time)

                print " * building array for first simulated period...",
                start_time = time.time()

                #TODO: this whole process of merging all periods is very
                # opinionated and does not allow individuals to die/disappear
                # before the simulation starts. We couldn't for example,
                # take the output of one of our simulation and
                # re-simulate only some years in the middle, because the dead
                # would be brought back to life. In conclusion, it should be
                # optional.
                entity.array, entity.id_to_rownum = \
                    buildArrayForPeriod(table.table, entity.fields,
                                        entity.input_rows,
                                        entity.input_index, start_period)
                print "done (%s elapsed)." % time2str(time.time() - start_time)
                entity.table = output_table
        except:
            input_file.close()
            output_file.close()
            raise

        return input_file, output_file, dataset['globals']


class Void(DataSource):
    def __init__(self, output_path):
        self.output_path = output_path

    def run(self, globals_fields, entities, start_period):
        output_file = tables.openFile(self.output_path, mode="w")
        output_entities = output_file.createGroup("/", "entities", "Entities")
        for entity in entities.itervalues():
            dtype = np.dtype(entity.fields)
            entity.array = np.empty(0, dtype=dtype)
            entity.id_to_rownum = np.empty(0, dtype=int)
            output_table = output_file.createTable(
                output_entities, entity.name, dtype,
                title="%s table" % entity.name)

            entity.input_table = None
            entity.table = output_table
        return None, output_file, None


def populate_registry(fpath):
    import entities
    import registry
    h5in = tables.openFile(fpath, mode="r")
    for table in h5in.root.entities:
        registry.entity_registry.add(entities.Entity.from_table(table))
    return h5in
