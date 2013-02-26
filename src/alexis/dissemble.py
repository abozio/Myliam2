from munkres import Munkres, print_matrix

matrix =[[  7.2115 , 31.9857 ,  6.8306],
         [  6.1489 ,  5.6583 ,  6.461 ],
         [ 32.9857 , 32.9857 , 32.9857]]
print type(matrix)
m = Munkres()
indexes = m.compute(matrix)
print_matrix(matrix, msg='Lowest cost through this matrix:')
total = 0
for row, column in indexes:
    value = matrix[row][column]
    total += value
    print '(%d, %d) -> %d' % (row, column, value)
print 'total cost: %d' % total
print matrix