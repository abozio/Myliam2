from munkres import Munkres, print_matrix
import numpy as np

matrix =[[  7.2115 , 31.9857 ,  6.8306],
         [  6.1489 ,  5.6583 ,  6.461 ],
         [ 32.9857 , 32.9857 , 32.9857]]

def rapid(matrix):
    m = Munkres()
    indexes = m.compute(matrix)
    print_matrix(matrix, msg='Lowest cost through this matrix:')
    total = 0
    for row, column in indexes:
        value = matrix[row][column]
        total += value
        print '(%d, %d) -> %d' % (row, column, value)
    print 'total cost: %d' % total

rapid(matrix)

mat1 = [[  1 ,  5 ,  6 ],
         [ 4 ,  5 ,  1 ],
         [ 6 ,  3 ,  7 ]]
rapid(mat1)

print mat1[2]
print  type(mat1)