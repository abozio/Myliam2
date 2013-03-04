
#!/usr/bin/python
# ecole polytechnique - c.durr - 2009

# Kuhn-Munkres, The hungarian algorithm.  Complexity O(n^3)
# Computes a max weight perfect matching in a bipartite graph
# for min weight matching, simply negate the weights.

""" Global variables:
       n = number of vertices on each side
       U,V vertex sets
       lu,lv are the labels of U and V resp.
       the matching is encoded as 
       - a mapping Mu from U to V, 
       - and Mv from V to U.
    
    The algorithm repeatedly builds an alternating tree, rooted in a
    free vertex u0. S is the set of vertices in U covered by the tree.
    For every vertex v, T[v] is the parent in the tree and Mv[v] the
    child.

    The algorithm maintains minSlack, s.t. for every vertex v not in
    T, minSlack[v]=(val,u1), where val is the minimum slack
    lu[u]+lv[v]-w[u][v] over u in S, and u1 is the vertex that
    realizes this minimum.

    Complexity is O(n^3), because there are n iterations in
    maxWeightMatching, and each call to augment costs O(n^2). This is
    because augment() makes at most n iterations itself, and each
    updating of minSlack costs O(n).
    """

def improveLabels(val):
    """ change the labels, and maintain minSlack. 
    """
    for u in S:
        lu[u] -= val
    for v in V:
        if v in T:
            lv[v] += val
        else:
            minSlack[v][0] -= val

def improveMatching(v):
    """ apply the alternating path from v to the root in the tree. 
    """
    u = T[v]
    if u in Mu:
        improveMatching(Mu[u])
    Mu[u] = v
    Mv[v] = u

def slack(u,v):  return lu[u]+lv[v]-w[u][v]

def augment():
    """ augment the matching, possibly improving the lablels on the way.
    """
    while True:
        # select edge (u,v) with u in S, v not in T and min slack
        ((val, u), v) = min([(minSlack[v], v) for v in V if v not in T])
        assert u in S
        if val>0:        
            improveLabels(val)
        # now we are sure that (u,v) is saturated
        assert slack(u,v)==0
        T[v] = u                            # add (u,v) to the tree
        if v in Mv:
            u1 = Mv[v]                      # matched edge, 
            assert not u1 in S
            S[u1] = True                    # ... add endpoint to tree 
            for v in V:                     # maintain minSlack
                if not v in T and minSlack[v][0] > slack(u1,v):
                    minSlack[v] = [slack(u1,v), u1]
        else:
            improveMatching(v)              # v is a free vertex
            return



def maxWeightMatching(weights):
    """ given w, the weight matrix of a complete bipartite graph,
        returns the mappings Mu : U->V ,Mv : V->U encoding the matching
        as well as the value of it.
    """
    
    # Create a square matrix if needed
    d = len(weights) - len(weights[0]) 
    if d > 0 :
        for i in range(len(weights)):
            weights[i].extend([0] * d)
    if d < 0 :
        for i in range(-d):
            weights.append([0] * len(weights[0]))

    global U,V,S,T,Mu,Mv,lu,lv, minSlack, w
    w = list(weights)
    for i in range(len(weights)):
        w[i] = map(lambda x: x*-1, weights[i])   
    n  = len(w)
    U  = V = range(n)
    lu = [ max([w[u][v] for v in V]) for u in U]  # start with trivial labels
    lv = [ 0                         for v in V]
    Mu = {}                                       # start with empty matching
    Mv = {}
    while len(Mu)<n:
        free = [u for u in V if u not in Mu]      # choose free vertex u0
        u0 = free[0]
        S = {u0: True}                            # grow tree from u0 on
        T = {}
        minSlack = [[slack(u0,v), u0] for v in V]
        augment()
    #                                    val. of matching is total edge weight
    val = sum(lu)+sum(lv)
    return Mu

  
#  a small example 

print maxWeightMatching([[1,2,100,4],[3,6,9,12],[4,8,12,16]])

import numpy as np
import time as t
n = 100
mat = (1000000*np.random.rand(n,n)).round()
mat = mat.tolist()
#print mat
print maxWeightMatching(mat)

# read from standard input a line with n
# then n*n lines with u,v,w[u][v]

def temps(n,m):
    temps = []
    for i in range(n,m):
        k = i * 100
        mat = (1000000*np.random.rand(k,k)).round()
        mat = mat.tolist()        
        start = t.clock()
        maxWeightMatching(mat)
        temps.append(t.clock() - start)
        print "... etape %d sur %d : temps %d" %(i-n +1 ,m-n, (t.clock() - start))
    return temps
        
#voir=temps(5,15)
#print voir


def calc_expand(matrix,duplic):
    """
    duplic est un array 2 lignes et n colonnes
    matrix is a square matrix of size n
    
    On veut resoudre une matrice qui est matrix in which rows were multiplied
    by numbers in first row of duplic and columns as many times as 
    numbers on second row of duplic    
    """
    n = matrix.shape[0]
    calc_expand = {}
  
#    assert duplic.min()>0 
#    assert duplic[0].sum() == duplic[1].sum()
    assert len(duplic[0]) == len(duplic[0]) 
    assert len(duplic[0]) == n

    dupl = duplic
    etape = 0 
    while ( (dupl[0] > 0).any() & (dupl[1] > 0).any() ) : 
        # matrix the optimisation is made on
        a = np.where( dupl[0]==0)[0]
        b = np.where( dupl[1]==0)[0]
        who = np.delete(matrix, a.tolist(), 0)
        who = np.delete(who, b.tolist(), 1)
        row_remo = 0 + (dupl[0]==0)
        col_remo = 0 + (dupl[1]==0)
        # how many time to save it and for who
        minval = np.min(dupl[np.nonzero(dupl)])
        save = dupl.copy()
        for i in [0,1]:
            for j in range(n):
                save[i][j] = min(minval, dupl[i][j])
                dupl[i][j] = max(0, dupl[i][j]-minval)
        
        
        # Take into account fact that who is not the full matrix
        # TODO: put name directly in maxWeightMatching function to avoid that loop 
        list_ini = maxWeightMatching(who.tolist())
        list = {}

        row_remo = row_remo.cumsum()
        col_remo = col_remo.cumsum()
        for x,y in list_ini.items(): 
            # on travaille pour avoir les bons noms
            # c'est peut etre pas la bonne strategie pour l'instant.
            x_true = x + row_remo[x]
            y_true = y + col_remo[y] 
            del list_ini[x]
            list[x_true] = y_true
            
        calc_expand[etape] =  (list,save)
        etape += 1 
        # on se prepare a l etape suivante
        # The min value is subtracted from duplic except when zero
    calc_expand[etape] =  ({},dupl)
    return calc_expand

mat = np.arange(16).reshape(4,4)
duplic = np.array( [(3,6,4,3),(4,8,4,3)])

calc_expand(mat,duplic)

#
def temps_expand(n,m,non_expand=True):
    temps1 = []
    temps2 = []
    temps_expand1 = []
    temps_expand2 = []
    for i in range(n,m):
        k = i * 10
        mat = (1000000*np.random.rand(k,k)).round()
        duplic1 =  np.array([ [50] * k  , [50] * k ])
        duplic2 =  np.array( [ 1+np.arange(k) , 1+k+np.arange(k) ])
        start = t.clock()
        calc_expand(mat,duplic1)
        temps_expand1.append(t.clock() - start) 
               
        start = t.clock()
        calc_expand(mat,duplic2)
        temps_expand2.append(t.clock() - start) 
        
        mat = mat.tolist()        
        start = t.clock()
        maxWeightMatching(mat)
        temps1.append(t.clock() - start)     
          
        print "... etape %d sur %d" %(i-n +1 ,m-n)
    if non_expand:
        return temps1, temps2, temps_expand1, temps_expand2
    else :
        return temps_expand1, temps_expand2

voir =  temps_expand(14,20)
print voir
print voir[3]