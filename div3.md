So I wanted divmod(x, 3) using only bit operations for reasons unknown to me.

Mod 3 is pretty simple. If $x_n$ accesses the bit at place $n$, We can compress the number without changing the modulus like:

$\sum_{i=0}^{i+=2} x_i - \sum_{i=1}^{i+=2} x_i$ \% 3 = x\%3$

This is constant time for fixed-precision ints!

        def mod3(x):
            oe = 0,0
            i = 1
            while x < 0:
                oe[i] = x&1
                i ^= 1
                x>>=1
            return (oe[0] - oe[1]) % 3

The most intuitive approach for dividing is recursive. It takes advantage of 

$\frac{1}{3} = \frac{1}{4} + \frac{1}{16} + \frac{1}{64} + \dots$

        def div3(x):
            s = x>>2
            return s + div3(s+(x&3)) \ 
                   if x>5 else       \
                   1 if x>2 else 0 
                   
                   
I suspected better accuracy for the floor of x/3 could be found by approaching from over, not under. The following iterative algorithm was not much more accurate, ultimately. This essentially stems from $\frac{1}{3} = \frac{1}{2} - \frac{\frac{1}{3}}{2}$.

        def div3(x):
            def lg(n): return 0 if n<2 else 1 + lg(n>>1)
            s = 1
            q = lg(x)
            x<<=q
            x>>=1
            e = x
            while x>3:
                d,m = x>>2, x&3
                e -= d 
                x = d+m
            return (e>>q)
