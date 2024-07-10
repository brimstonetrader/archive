    import numpy as np


    def neighbors(n, x):
    return ([x+n] if x+n<n*n else []) + \
           ([x-n] if x>=n    else []) + \
           ([x-1] if x%n>0   else []) + \
           ([x+1] if x%n<n-1 else [])


    def decompose(n, g):
        if n*n<g: return []
        spawn = [i for i in np.random.choice(n*n, g, replace=False)]
        grid = [0] * (n*n)
        nbhd = [[] for _ in range(g)]
        occupied = set(spawn)
        for i,x in enumerate(spawn): 
            grid[x] = i+1
            for u in neighbors(n, x): 
                if u not in occupied: nbhd[i].append(u)
        grp = 0
        while len(occupied) < len(grid):
            ns = nbhd[grp][:]
            d = 0
            for j,nb in enumerate(ns): 
                if nb in occupied: del nbhd[grp][j-d] ; d+=1
            if nbhd[grp] != []:
                r = np.random.randint(len(nbhd[grp]))
                c = nbhd[grp][r]
                del nbhd[grp][r]
                grid[c] = grp + 1
                occupied.add(c)
                for nb in neighbors(n, c):
                    if nb not in occupied:
                        nbhd[grp].append(nb)
                # for i in range(5): print(grid[5*i:5*i+5])
                # print()
                # print(nbhd)
                # print()
                # print('occupied: ', occupied)
            grp = (grp + 1) % g
        return grid
    
    z = decompose(10,5)
    for i in range(10): print(z[10*i:10*(i+1)])


    def partitiongrid(n, gr):
        g = decompose(n, gr)
        w = 2*n-1
        print(' _' * n)
        s = ''
        for i in range(n):
            s = '|'
            for j in range(n-1):
                s += '_' if i == n-1 or g[n*i + j] != g[n*(i+1)+j] else ' '
                s += '|' if g[n*i + j] != g[n*i+1+j] else ' '
            s += '_' if i == n-1 or g[n*i + n-1] != g[n*(i+1)+n-1] else ' '
            s += '|'
            print(s)
            s = ''
        print()
        # for i in range(n): print(g[n*i:n*(i+1)])
    partitiongrid(15, 10)

    ->   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
        |    _|        _ _ _|         |
        |   |      _ _|     |         |
        |   |     |_        |_ _      |
        |   |_ _ _ _|          _|_    |
        |  _|  _|   |_ _ _ _ _|   |_  |
        | |   |_ _ _       _|       |_|
        | |_        |    _|           |
        |   |       |_  |_            |
        |_ _|  _ _ _ _|   |_ _ _     _|
        |   |_|   |  _ _ _ _ _| |_ _| |
        |         |_|         |       |
        |         |           |       |
        |         |          _|       |
        |         |         |         |
        |_ _ _ _ _|_ _ _ _ _|_ _ _ _ _| (0.0s)

        
