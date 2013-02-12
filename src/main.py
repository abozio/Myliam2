# -*- coding:utf-8 -*-

import argparse
from os.path import splitext

import yaml

from simulation import Simulation
from importer import csv2h5
from console import Console
from data import populate_registry
from registry import entity_registry

__version__ = "0.5.0"


class AutoflushFile(object):
    def __init__(self, f):
        self.f = f

    def write(self, s):
        self.f.write(s)
        self.f.flush()


def eat_traceback(func, *args, **kwargs):
# e.context      | while parsing a block mapping
# e.context_mark | in "import.yml", line 18, column 9
# e.problem      | expected <block end>, but found '<block sequence start>'
# e.problem_mark | in "import.yml", line 29, column 12
    try:
        try:
            return func(*args, **kwargs)
        except Exception, e:
            try:
                import traceback
                #TODO: use config.output_directory. The problem is that it
                # might not be available at this point and it's only valid
                # for run and explore commands
                with file('error.log', 'w') as f:
                    traceback.print_exc(file=f)
            except IOError, log_ex:
                print "WARNING: %s on '%s'" % (log_ex.strerror,
                                               log_ex.filename)
            except Exception, log_ex:
                print log_ex
            raise e
    except yaml.parser.ParserError, e:
        # eg, inconsistent spacing, no space after a - in a list, ...
        print "SYNTAX ERROR %s" % str(e.problem_mark).strip()
    except yaml.scanner.ScannerError, e:
        # eg, tabs, missing colon for mapping. The reported problem is
        # different when it happens on the first line (no context_mark) and
        # when it happens on a subsequent line.
        if e.context_mark is not None:
            if e.problem == "could not found expected ':'":
                msg = "could not find expected ':'"
            else:
                msg = e.problem
            mark = e.context_mark
        else:
            if (e.problem ==
                "found character '\\t' that cannot start any token"):
                msg = "found a TAB character instead of spaces"
            else:
                msg = ""
            mark = e.problem_mark
        if msg:
            msg = ": " + msg
        print "SYNTAX ERROR %s%s" % (str(mark).strip(), msg)
    except yaml.reader.ReaderError, e:
        if e.encoding == 'utf8':
            print "\nERROR in '%s': invalid character found, this probably " \
                  "means you have used non ASCII characters (accents and " \
                  "other non-english characters) and did not save your file " \
                  "using the UTF8 encoding" % e.name
        else:
            raise
    except SyntaxError, e:
        print "SYNTAX ERROR:", e.msg.replace('EOF', 'end of block')
        if e.text is not None:
            print e.text
            offset_str = ' ' * (e.offset - 1) if e.offset > 0 else ''
            print offset_str + '^'
    except Exception, e:
        print "\nERROR:", str(e)


def simulate(args):
    print "Using simulation file: '%s'" % args.file
    print args.file
    print args.input_path
    print args.output_path
    print args.input_file
    print args.output_file
    print args.interactive
    simulation = Simulation.from_yaml(args.file,
                                      input_dir=args.input_path,
                                      input_file=args.input_file,
                                      output_dir=args.output_path,
                                      output_file=args.output_file)
    simulation.run(args.interactive)
#    import cProfile as profile
#    profile.runctx('simulation.run()', vars(), {},
#                   'c:\\tmp\\simulation.profile')
    # to use profiling data:
    # import pstats
    # p = pstats.Stats('c:\\tmp\\simulation.profile')
    # p.strip_dirs().sort_stats('cum').print_stats(30)


def explore(fpath):
    _, ext = splitext(fpath)
    ftype = 'data' if ext in ('.h5', '.hdf5') else 'simulation'
    print "Using %s file: '%s'" % (ftype, fpath)
    if ftype == 'data':
        h5in = populate_registry(fpath)
        h5out = None
        entity, period = None, None
    else:
        simulation = Simulation.from_yaml(fpath)
        h5in, h5out, periodic_globals = simulation.load()
        ent_name = simulation.default_entity
        entity = entity_registry[ent_name] if ent_name is not None else None
        period = simulation.start_period + simulation.periods - 1
    try:
        c = Console(entity, period)
        c.run()
    finally:
        h5in.close()
        if h5out is not None:
            h5out.close()


class PrintVersionsAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        import numpy
        import numexpr
        import carray
        import tables

        print '''numpy {np}
numexpr {ne}
pytables {pt}
carray {ca}
pyyaml {yml}'''.format(np=numpy.__version__,
                       ne=numexpr.__version__,
                       pt=tables.__version__,
                       ca=carray.__version__,
                       yml=yaml.__version__)
        parser.exit()


def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('--versions', action=PrintVersionsAction, nargs=0,
                        help="display versions of dependencies")
    parser.add_argument('--input-path', dest='input_path',
                        help='override the input path')
    parser.add_argument('--input-file', dest='input_file',
                        help='override the input file')
    print parser
    parser.add_argument('--output-path', dest='output_path',
                        help='override the output path')
    parser.add_argument('--output-file', dest='output_file',
                        help='override the output file')

    subparsers = parser.add_subparsers(dest='action')
  
    # create the parser for the "run" command
    parser_run = subparsers.add_parser('run', help='run a simulation')
    parser_run.add_argument('file', help='simulation file')
    parser_run.add_argument('-i', '--interactive', action='store_true',
                            help='show the interactive console after the '
                                 'simulation')

    # create the parser for the "import" command
    parser_import = subparsers.add_parser('import', help='import data')
    parser_import.add_argument('file', help='import file')

    # create the parser for the "explore" command
    parser_import = subparsers.add_parser('explore', help='explore data of a '
                                          'past simulation')
    parser_import.add_argument('file', help='explore file')

    parsed_args = parser.parse_args()
#   action_funcs = {'run': simulate, 'import': csv2h5, 'explore': explore}
#   action_funcs[parsed_args.action](parsed_args)

    action = parsed_args.action
    fpath = parsed_args.file
    if action == 'run':
        simulate(parsed_args)
    elif action == "import":
        csv2h5(fpath)
    elif action == "explore":
        explore(fpath)

if __name__ == '__main__':
    import sys
    import platform

    sys.stdout = AutoflushFile(sys.stdout)
    sys.stderr = AutoflushFile(sys.stderr)
    print "LIAM2 %s using Python %s (%s)" % (__version__,
                                             platform.python_version(),
                                             platform.architecture()[0])
    
    for arg in sys.argv:
        print arg
    print "\n".join(sys.argv)
 

#    eat_traceback(main)
    main()
