One of the big operations John Conway uses for Surreal Numbers and Game Theory is calculating the mex of 
a set of naturals. This is short for "minimal excluded element". mex(1,5,7,2,3) = 4. It is quite simple. 
Let us ask a reasonable question. What is the average mex of a set of $n$ random numbers, selected from an
interval spanning from 1 to $n$, inclusive? The first two cases are simple to work out by hand:

mexavg(1) = 2, as the only possible set is [1].

mexavg(2) = 2.25, four equally likely sets, [[1,1],[1,2],[2,1],[2,2]], have mexes of [2,3,3,1], which 
averages to 2.25.

I wrote a bit of code instead of analyzing the 27 3-cases by hand. With this data, as demonstrated below, 
I conjecture that this value approaches $e$ for sufficiently large values of $n$.

       ghci> mexOfNRand 527987395 5
  
	    2.357142857142857
 
       ghci> mexOfNRand 527987395 10
  
	    2.6470588235294117
 
       ghci> mexOfNRand 527987395 20
  
	    2.7132169576059852
 
       ghci> mexOfNRand 527987395 40
  
	    2.748906933166771
 
       ghci> mexOfNRand 527987395 80
  
	    2.752382440243712

       ghci> mexOfN 527987395 160
   
 	    2.734229131674544

That's no fun, though, I want to know why. Let's write a slower function that gets exact values.

		> mN :: Int -> Double 
		> mN n = avg (mexEach n (allPossibleSequences n) )

		> rep :: Int -> Int -> [Int]
		> rep x 0 = []
		> rep x n = x:(rep x (n-1))

		> allPossibleSequences :: Int -> [[Int]]
		> allPossibleSequences n = aPS n 1 (n^n) 

		> aPS :: Int -> Int -> Int -> [[Int]]
		> aPS n a b  = if a>b then [] else (expand a 0 n):(aPS n (a+1) b)

		> expand :: Int -> Int -> Int -> [Int]
		> expand a i n = if i==n then [] else (a%n+1):(expand (a//n) (i+1) n)

		> mexEach2 :: Int -> [[Int]] -> [Double]
		> mexEach2 _ [] = []
		> mexEach2 n (ss:sss) = (mex ss 1):(mexEach2 n sss)


		> (/%/) :: Int -> Int -> (Int, Int)
		> a /%/ b = a `divMod` b

		> (%) :: Int -> Int -> Int
		> a % b = a `mod` b

		> (//) :: Int -> Int -> Int
		> a // b = a `div` b

-----------------------------------------------

		ghci> mN 1
		2.0                = 2^1 / 1^1
		ghci> mN 2
		2.25               = 3^2 / 2^2
		ghci> mN 3        
		2.3703703703703702 = 4^3 / 3^3
		ghci> mN 4
		2.44140625         = 5^4 / 4^4
		ghci> mN 5
		2.48832            = 6^5 / 5^5
		ghci> mN 6
		2.5216263717421126 = 7^6 / 6^6
		ghci> mN 7 
		2.546499697040713  = 8^7 / 7^7

Note that lim (n→∞) (n+1)^n / n^n = lim (n→∞) (n+1 / n)^n = lim (n→∞) (1 + 1/n)^n, which is the most 
classic and well known of the e definitions. That the denominator holds is trivial. 
		

   
		> inst :: Int -> Int
		> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

		> rng :: Int -> Int -> [Int]
		> rng ornd 1  = [(inst ornd)]
		> rng ornd it = do 
		>   let nrnd = inst ornd 
		>   (nrnd : rng nrnd (it-1))

		> listMod :: [Int] -> Int -> [Int]
		> listMod [] _ = []
		> listMod (a:as) b = (((a `div` 79) `mod` b)+1):(listMod as b)

		> mexOfNRand :: Int -> Int -> Double
		> mexOfNRand seed n = do 
		>   let rnds = (listMod (rng seed (n^3)) n)
		>   avg (mexEach n rnds)

		> mexEach :: Int -> [Int] -> [Double]
		> mexEach _ [] = []
		> mexEach n rnds = do 
		>   let (as,bs) = splitAt n rnds
		>   (mex as 1):(mexEach n bs)

		> mex :: [Int] -> Int -> Double
		> mex as m = if m `elem` as then (mex as (m+1)) else realToFrac m

		> avg :: [Double] -> Double
		> avg ds = (sum' ds) / (realToFrac (len ds))

		> sum' :: [Double] -> Double
		> sum' []     = 0.0
		> sum' (d:ds) = d+(sum' ds)

		> len :: [a] -> Int
		> len []     = 0
		> len (a:as) = 1+(len as)
