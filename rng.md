rng

In this section we will be exploring randomness. We will start by defining 
permuting and sorting, generate some pseudo-random numbers, glance at some 
quantum voodoo, and end up with a pleasant little game.

A list is an unordered multiset of objects with the same type. A list is 
sorted when for every element x_i ϵ xs, x_j ϵ xs | j>i -> x_i <= x_j. 
There are four or five methods for doing so.

Merge Sort

> mergeSort :: [Integer] -> [Integer]
> mergeSort      [] = []
> mergeSort     [x] = [x]
> mergeSort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> mergeSort list    = collate (mergeSort a) (mergeSort b)
>      where (a, b) = splitAt ((length(list)) `div` 2) list

If the list has three or more elements, we split it, sort both, then 
collate both. Haskell integer division is such that, if the list has 
an odd number of elements, the first will be larger.

> collate :: [Integer] -> [Integer] -> [Integer]
> collate        as []  = as
> collate (a:as) (b:bs) = if (a>b) then b:collate (a:as) bs 
>                                  else a:collate as (b:bs)

These two lists are sorted, so either a or b will be our smallest 
element.

Quick Sort

A similar idea, and an almost identical initial six lines.

> quickSort :: [Integer] -> [Integer]
> quickSort      [] = []
> quickSort     [x] = [x]
> quickSort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> quickSort list    = do
>   let (as, b:bs)  = splitAt ((length(list)) `div` 2) list
>   let [cs, ds]    = pivotAbout b as [[],[]]
>   let [es, fs]    = pivotAbout b bs [[],[]]
>   (quickSort (cs++es)) ++ (b:(quickSort (ds++fs)))

We pivot about the middle element, calling a new function that will cleave the list into two: one less, one greater, and put the two into that empty list we give it. We then call quickSort again, on both halves, ordering them correctly.

> pivotAbout :: Integer -> [Integer] -> [[Integer]] -> [[Integer]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>                          then [x:a,b] 
>                          else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>                          then pivotAbout n xs [x:a,b] 
>                          else pivotAbout n xs [a,x:b]

These two algorithms are both n(log(n))ish, as they both work by splitting the initial lists in two as many times as necessary. I used the below function to test them.

> list1ton :: Integer -> [Integer]
> list1ton n | n<1 = []
> list1ton n = n:(list1ton (n-1))

> lcg2 :: Int -> Int -> [Int]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> permute :: Int -> [Int] -> [Int]
> permute seed as = do 
>   let bs = lcg2 seed (len as)
>   permute2 as bs []

> permute2 :: [Int] -> [Int] -> [Int] -> [Int]
> permute2 as []     cs = cs 
> permute2 as (b:bs) cs = do
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

The most common algorithmic (i.e. fake) random number generator goes by the 
longname 

Linear Congruential Generator. 

The basic equation is x_2 = (a*x_1 + c) % m.
 
  The game is to make m as big as possible, and 
  ensure it's got no factors other than two.

    You should make c prime, ideally, or at the 
    very least coprime to m.

    Then, if you ensure these two things, the 
    resulting sequence will only repeat once 
    every m times:

      1. a-1 `div` f == 0 for all prime factors f of m
      2. a-1 `mod` 4 == 1 if m `div` 4 == 0

These particular parameters are Word of Knuth.

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))


The first pseudorandom number generator comes courtesy of noted fed John Von 
Neumann. The meat of his method is an iterative process, but one that eventually 
tends to zero. Not cool Johnny! He squares a seed number with x many digits, rips 
out x many digits from the middle, squares that, yada yada. 

> lenn :: Int -> Int
> lenn i = if i<1 then 0 else 1 + (lenn (i `div` 10))

> inst2 :: Int -> Int
> inst2 0        =  0
> inst2 r        =  do
>   let rr       =  r*r
>   let (lr,lrr) = (lenn r,lenn rr)
>   let x        = (lrr - lr) `div` 2
>   (rr `div` (10^x)) `mod` (10^lr)

> msm :: Int -> Int -> [Int]
> msm ornd 1 = [(inst2 ornd)]
> msm ornd it = do 
>   let nrnd = inst2 ornd 
>   if nrnd == ornd then [ornd] else (nrnd : msm nrnd (it-1))

In this case, the middle square method peters out after 88 iterations, while 
the linear congruential method carries on unfazed.

ghci> lcg 314159265 100
    [1811288460,2039979899,1706036894, ..., 2887701979,630722942,1903037125]
ghci> msm 314159265 100
    [604378534,341235998,200633105, ... 7746,5,5]

In general, we will use the linear congruential method because of its speed 
and effectiveness. Here are some scattered other methods:

Lehmer RNG 

This is an LCG with one term removed. It's better than MSM. The two numbers 
should be coprime.

> inst3 :: Int -> Int
> inst3 r = (16807*r) `mod` ((2 ^ 31)-1) 

> lrng :: Int -> Int -> [Int]
> lrng ornd 1  = [(inst3 ornd)]
> lrng ornd it = do 
>   let nrnd = inst3 ornd 
>   (nrnd : lrng nrnd (it-1))

For our next method to work we will need to talk about bitwise operations on
integers. A bit is a zero or a one. Using them, we can write any number in base 
two, like 

1    7    3  =  173_10 = 10101101_2 = 1   0   1   0   1   1   0   1

100  10   1                           128 64  32  16  8   4   2   1 

10^2 10^1 10^0                        2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0

A binary operation is something like plus or divide, that takes in two things 
and outputs one. There are 16 possible binary operations on bits. I will present 
three, and code one.

OR AND XOR

  AND   \    OR   \     XOR
0&0 = 0 \ 0|0 = 0 \ 0~0 = 0 
0&1 = 1 \ 0|1 = 1 \ 0~1 = 1 
1&0 = 1 \ 1|0 = 1 \ 1~0 = 1 
1&1 = 1 \ 1|1 = 1 \ 1~1 = 1 

We can do each of these operations on two full integers, by applying the operation 
to each bit in the binary representation of the numbers. Thus, 

14 ~ 11 = 1110 ~
          1011 = 
          0101_2 = 5_10

> (~) :: Int -> Int -> Int
> (~) a 0 = a
> (~) 0 b = b
> (~) a b = do
>   let (da,ma) = a `divMod` 2
>   let (db,mb) = b `divMod` 2
>   if ma == mb 
>     then 2*(da ~ db)
>     else 1+(2*(da ~ db))

It is simple to code similar operations for OR and AND. With XOR, we can create 
a Lagged Fibonacci Generator.

The root equation is x_n = (x_(n-j) <op> x_(n-k)) % m

m is usually 2^2^x where x is something or other.
 
y = x^k + x^j + 1 needs to be irreducible (unfactorable) and primitive mod 2.

Here are some (j,k)-tuples satisfying that condition, taken from page 29 of 
volume 2 of The Art of Computer Programming:

(24, 55),  (38, 89),   (37, 100),  (30, 127), 
(83, 258), (107, 378), (273, 607), (1029, 2281), 
(576, 3217), (4187, 9689), (7083, 19937), (9739, 23209)

(5,17) (6,31) and (7,10) also work, but that's not coming from Knuth.

> lfg :: Int -> Int -> [Int]
> lfg ornd it = lfg2 (lcg ornd 55) it

> lfg2 :: [Int] -> Int -> [Int] 
> lfg2 seeds 0 = []
> lfg2 seeds i = do 
>   let nrnd   = ((seeds !! ((len seeds)-24)) ~ (head seeds)) `mod` (2^32)
>   (nrnd : lfg2 (nrnd : take 54 seeds) (i-1))

Lastly, let us talk about Elementary Cellular Automata. These are iterative 
processes on a line of bits (which, if you remember, can be construed into 
regular numbers). Whether a bit becomes a one or zero is determined by its 
neighbors. In one dimension, there are three decisive actors: the left neighbor, 
right neighbor, and the cell itself. There re eight ways these can be arranged, 
so there are 256 possible rules. One of them, Rule 30, is notably chaotic, and
generates random numbers well.

Rule 30

30 in base 10 = 00011110 in base 2

111 110 101 100 011 010 001 000
 0   0   0   1   1   1   1   0

> inst4 :: Int -> Int
> inst4 0 = 0
> inst4 r = do 
>   let w = r `mod` 8 
>   if 0 < w && w < 5
>     then 1 + (2*(inst4 (r `div` 2)))
>     else     (2*(inst4 (r `div` 2)))


> rule30 :: Int -> Int -> [Int]
> rule30 ornd 1  = [(inst4 ornd)]
> rule30 ornd it = do 
>   let nrnd = inst4 ornd 
>   (nrnd : lcg nrnd (it-1))

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

Inversive Congruential Generator



> icg :: Int -> Int -> Int -> Int -> Int 
> icg seed q a c = if seed == 0 then c else (a*(mmi a seed q)+c)%q

> mmi :: Int -> Int -> Int -> Int 
> mmi a x m = if (a * x) % m == 1 then x else mmi a (x+1) m

TODO:
	INVERSIVE CONGRUENTIAL GENERATOR
	BLUM BLUM SHUB
	ACORN
	MIXMAX
	KISS
	MERSENNE TWISTER
	XORSHIFT
	LINEAR-FEEDBACK SHIFT REGISTER

-- chapter 6 of schaum's probability

-- BINOMIAL

-- Suppose there exists an event with probability p of success and 1-p=q of failure. The probability of k successes in n trials is 

-- (n choose k) * (p ^ k) * (q ^ (n-k))

--  ^- there are this many ways of arranging k successes among n trials
--                 ^- the last two terms represent the probability for
--                    any one arrangement.

-- mu      = np
-- sigma^2 = npq

> d :: Integer -> Double
> d x = realToFrac x

> i :: Double -> Integer
> i x = round x 

> choose :: Integer -> Integer -> Integer
> choose n k = (fall n k) `div` (fact (n-k))

> fact :: Integer -> Integer
> fact 0 = 1
> fact 1 = 1
> fact n = n*(fact (n-1))

> fall :: Integer -> Integer -> Integer
> fall n k = if n==k then 1 else n*(fall (n-1) k)

> binompdf :: Integer -> Integer -> Double -> Double
> binompdf n k p = (itod (choose n k)) * ( p ** (d k)) * ((1.0-p) ** (d (n-k)))

ghci> binompdf 3 0 0.5
0.125
ghci> binompdf 3 1 0.5
0.375
ghci> binompdf 3 2 0.5
0.375
ghci> binompdf 3 3 0.5
0.125

-- As n grows bigger, this approaches a normal distribution. That looks like

--         1/
-- _________________ e^(-0.5 * (x-mu)^2 / sigma^2)
-- sigma * (2pi)^0.5



-- APPROXIMATING PI

-- The classical approach is to repeat the first three odd numbers twice, 
-- cut the list in half, and turn it into a fraction.

-- 355/113 = 3.1415929203539825

-- The continued fraction for pi shows that this is probably the best 
-- way to do it this tersely. One I find neat, if worse, is

-- (2 ** 0.5) + (3 ** 0.5) = 3.1462643699419726

-- Here's a modern, iterative approach.

> r :: Integer -> Double
> r x = (realToFrac x) ** 0.5

> y0 :: Double
> y0 = (r 2) - 1

> a0 :: Double
> a0 = 6 - (4 * (r 2))

> f :: Double -> Double 
> f y = (1 - (y ** 4)) ** 0.25

> borwein :: Integer -> Double 
> borwein i = borwein2 0 i y0 a0

> borwein2 :: Integer -> Integer -> Double -> Double -> Double
> borwein2 i j y a = if i==j then 1/a else do 
>   let y' = (1 - (f y))/(1 + (f y))
>   let a' = (a*((1 + y') ** 4)) - (2 ** (itod (2*i+3)))*y'*(1+y'+(y'**2))
>   borwein2 (i+1) j y' a'

-- borwein 3 = 3.141592653589792

-- This method converges like lightning.

-- More:

--   3.8,29,44 in base 60
--   22/7 (classic)
--   31 ** (1 / 3)
--  (2143 / 22) ** 0.25


-- e is kinder. The Taylor series is wicked easy.

> e :: Integer -> Double
> e i = if i==0 then 1 else (1/(itod (fact i))) + (e (i-1))

> poisson :: Integer -> Double -> Double -> Double 
> poisson k lmb = do 
>   let exp = e 20
>   lmb**(d k) * e**(-lmb) / (d (fact k))


> lcg2 :: Int -> Int -> [Int]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> permute :: Int -> [Int] -> [Int]
> permute seed as = do 
>   let bs = lcg2 seed (len as)
>   permute2 as bs []

> permute2 :: [Int] -> [Int] -> [Int] -> [Int]
> permute2 as []     cs = cs 
> permute2 as (b:bs) cs = do
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 





































> (%) :: Integer -> Integer -> Integer
> a % b = a `mod` b

> (//) :: Integer -> Integer -> Integer
> a // b = a `div` b

> (/%/) :: Integer -> Integer -> (Integer, Integer)
> a /%/ b = a `divMod` b

-- The central limit theorem means we can also just do an Irwin-Hall cop-out.

normal :: Double -> Double -> Integer -> Double 
normal m s2 seed = do 
  let rs12 = rng seed 12 10000
  (((realToFrac (sum rs12)) / 10000) - 6)*(s2**0.5) + m









