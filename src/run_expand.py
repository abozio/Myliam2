import tables


chemin = 'T:\\Myliam2\\Patrimoine\\duplication\\'
fpath=chemin+'simple2009.h5'
input_file = tables.openFile(fpath, mode="r")
print input_file.root