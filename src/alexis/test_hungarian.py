from munkres import Munkres, print_matrix
import numpy as np
import operator
import time as t


import pdb

matrix =[[  7.2115 , 31.9857 ,  6.8306],
         [  6.1489 ,  5.6583 ,  6.461 ],
         [ 32.9857 , 32.9857 , 32.9857]]

def rapid(matrix):
    if isinstance(matrix,np.ndarray):
        matrix=matrix.tolist() 
    m = Munkres()
    indexes = m.compute(matrix)
    print_matrix(matrix, msg='Lowest cost through this matrix:')
    total = 0
    for row, column in indexes:
        value = matrix[row][column]
        total += value
        print '(%d, %d) -> %d' % (row, column, value)
    print 'total cost: %d' % total

#rapid(matrix)

mat1 = np.array( [ (1 ,  8 ,  6),
                   (4 ,  5 ,  1) ,
                   (6 ,  3 ,  7) 
                   ] )
duplic = np.array( [( 3 , 5 , 1),
                   ( 2, 2 , 5 )
                   ])
                  

def expand(mat1,duplic):
    duplic1 = duplic.tolist()
    mat2=  mat1.repeat(duplic1[0],axis=0).repeat(duplic1[1],axis=1).tolist()
    m = Munkres()
    indexes = m.compute(mat2)
    print_matrix(mat2, msg='Lowest cost through this matrix:')
    total = 0
    old_row = duplic[0].cumsum()-1
    old_col = duplic[1].cumsum()-1
    for row, column in indexes:
        value = mat2[row][column]
        total += value
        print 'new (%d, %d) is old (%d, %d) -> %d' % (row, column, 
                                                      old_row.searchsorted(row), 
                                                      old_col.searchsorted(column),
                                                       value)
    print 'total cost: %d' % total
    n = Munkres()
    indexesn = n.compute(mat1)
    print 'au debut on avait :' 
    for row, column in indexesn:
        value = mat1[row][column]
        total += value
        print '(%d, %d) -> %d' % (row, column, value)    
    

#
#rapid(mat1)
#expand(mat1,duplic)


 
def time(f, *args):
    start = t.clock()
    apply(f, list(args))
    return t.clock() - start


def munk(matrix):
    m = Munkres()
    indexes = m.compute(matrix)

def calcul_time(n,m):
    calcul_time=[]
    for i in range(n,m):
        taille = 100*i 
        mat=np.floor(np.random.rand(taille,taille)*100000000000)
        print mat
        mat1=mat.tolist() 
        calcul_time.append(time(munk, mat1))
    return calcul_time

#voir = calcul_time(4,9)
#print voir


def munk_expand(matrix,duplic):
    """ 
    doit utiliser une matrix issue d'une 
    dupplicationn par duplic par exemple avec :
    duplic1 = duplic.tolist()
    matrix=  mat1.repeat(duplic1[0],axis=0).repeat(duplic1[1],axis=1).tolist()
    """
    m = Munkres()
#    indexes = m.compute(matrix)
    print duplic == 2
    print np.where(duplic == 2, 1, 0)
    print duplic.min()
    print duplic == duplic.min()
    print np.where(duplic == duplic.min(), 1, 0)
    a = np.where(duplic == duplic.min(), 1, 0)
    a = 1- a
    print a
    print matrix
    
    voir=matrix
    
    index=duplic.argmin()
    print duplic.flat[index]
    old_row = duplic[0].cumsum()-1
    old_col = duplic[1].cumsum()-1
    print old_row
    print old_row-old_row    
    print matrix.repeat(a[0],axis=0).repeat(a[1],axis=1)
    print old_row*np.where(duplic == 2, 1, 0)


duplic1 = duplic.tolist()
matrix  =  mat1.repeat(duplic1[0],axis=0).repeat(duplic1[1],axis=1)
munk_expand( matrix, 
             duplic)
    
    

        