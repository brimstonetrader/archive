A quick one, for today.

One of the big operations John Conway uses for Surreal Numbers and Game Theory is calculating the mex of 
a set. This is short for "minimal excluded element". mex(1,5,7,2,3) = 4. It is quite simple. Let us ask a 
reasonable question. What is the mex of a set of $n$ random numbers, selected from an interval from [1,$n$],
inclusive. The first two cases are simple to work out by hand:

mexavg(1) = 2, as the only possible set is [1].

mexavg(2) = 2.25, four equally likely sets, [[1,1],[1,2],[2,1],[2,2]], have mexes of [2,3,3,1], which 
averages to 2.25.

I wrote a bit of code instead of analyzing the 27 3-cases by hand, with this data, as demonstrated below, 
I conjecture that this value approaches $e$.

ghci> mexOfN 527987395 5
2.357142857142857
ghci> mexOfN 527987395 10
2.6470588235294117
ghci> mexOfN 527987395 20
2.7132169576059852
ghci> mexOfN 527987395 40
2.748906933166771
ghci> mexOfN 527987395 80
2.752382440243712

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

		> mexOfN :: Int -> Int -> Double
		> mexOfN seed n = do 
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
