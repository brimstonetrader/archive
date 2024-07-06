RANDOM PARTITION

A generalization / dumbing-down of an algorithm I wrote a couple years ago. It partitions a $n$x$n$ square grid into $g$ regions. (I wish len(xs) were written |xs|, that'd be cool).

        import numpy as np 
        
        def neighbors(n, x): return ([x+n] if x+n<n*n else []) + \
                                    ([x-n] if x>=n    else []) + \
                                    ([x-1] if x%n>0   else []) + \
                                    ([x+1] if x%n<n-1 else [])

        
        def decompose(n, g):
            if n<g: return []
            spawn = [i for i in np.random.choice(n*n, g, replace=False)]
            grid = [0] * (n*n)
            nbhd = [[] for _ in range(g)] #hate this abt python
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
                grp = (grp + 1) % g
            return grid

        # z = decompose(10,5)
        # for i in range(10): print(z[10*i:10*(i+1)]) ->
        #    [4, 4, 4, 4, 1, 1, 1, 1, 1, 1]
        #    [4, 4, 4, 4, 1, 1, 1, 1, 1, 1]
        #    [4, 2, 2, 4, 4, 4, 3, 3, 1, 1]
        #    [2, 2, 2, 2, 4, 4, 3, 3, 1, 1]
        #    [2, 2, 2, 2, 4, 3, 3, 3, 3, 3]
        #    [2, 2, 2, 2, 2, 3, 3, 3, 3, 3]
        #    [2, 5, 5, 5, 5, 3, 3, 3, 3, 3]
        #    [5, 5, 5, 5, 5, 5, 5, 3, 3, 3]
        #    [5, 5, 5, 5, 5, 5, 5, 3, 3, 3]
        #    [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
        
Gonna use this to make a couple puzzles. Might have to translate it into Haskell.
