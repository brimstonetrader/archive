NIM

Back at Jimmy Johns we didn't have playing cards or TikTok or customers. We had two computers with which we could take orders, which had calculators. To entertain myself, when I wasn't making Mobius Strips from the used tape roll and tearing them in half lengthwise, I'd challenge my coworkers to Two-Pile Nim, allowing them to type in any two numbers of their choice onto the computers. I never told them, and they never figured out, the optimal strategy, which is to always end your turn with giving your opponent two piles of the same size. In general, you want the bxor (an associative and commutative operation) of all piles to be zero at the end of your turn.

    import numpy as np 
    
    def nimsum(xs): 
        return 0 if xs==[] else return xs[0] ^ nimsum(xs[1:])
    
    def remove(n, xs):
        b, i = 1, np.random.randint(len(xs))
        while b:
            if xs[i] < n: i = (i+1)%len(xs)
            else: xs[i] -= n ; b = 0
        return xs
    
    def nimbot(xs): 
        s = nimsum(xs)
        if s==0: return remove(1,xs) #Can't win unless opponent makes a mistake.
        else:
            for i in range(len(xs)):
                if ((xs[i] ^ s) < xs[i]): 
                    xs[i] = (xs[i] ^ s)
                    return xs
        return remove(1,xs)
    
    def perf_game(xs):
        if sum(xs) == 0: return
        else:
            print(xs, nimsum(xs))
            xs = nimbot(xs)
            perf_game(xs)
    
    perf_game([1,2,3,4,5,6,7])
    
    [1, 2, 3, 4, 5, 6, 7] 0
    [1, 2, 2, 4, 5, 6, 7] 1
    [0, 2, 2, 4, 5, 6, 7] 0
    [0, 2, 2, 3, 5, 6, 7] 7
    [0, 2, 2, 3, 2, 6, 7] 0
    [0, 2, 2, 3, 1, 6, 7] 3
    [0, 1, 2, 3, 1, 6, 7] 0
    [0, 0, 2, 3, 1, 6, 7] 1
    [0, 0, 2, 2, 1, 6, 7] 0
    [0, 0, 2, 1, 1, 6, 7] 3
    [0, 0, 1, 1, 1, 6, 7] 0
    [0, 0, 0, 1, 1, 6, 7] 1
    [0, 0, 0, 0, 1, 6, 7] 0
    [0, 0, 0, 0, 0, 6, 7] 1
    [0, 0, 0, 0, 0, 6, 6] 0
    [0, 0, 0, 0, 0, 5, 6] 3
    [0, 0, 0, 0, 0, 5, 5] 0
    [0, 0, 0, 0, 0, 5, 4] 1
    [0, 0, 0, 0, 0, 4, 4] 0
    [0, 0, 0, 0, 0, 3, 4] 7
    [0, 0, 0, 0, 0, 3, 3] 0
    [0, 0, 0, 0, 0, 2, 3] 1
    [0, 0, 0, 0, 0, 2, 2] 0
    [0, 0, 0, 0, 0, 1, 2] 3
    [0, 0, 0, 0, 0, 1, 1] 0
    [0, 0, 0, 0, 0, 0, 1] 1
