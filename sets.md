# OPERATIONS ON SETS

To find the intersection of two sets we extract all those elements from as that are also in bs.

    > intersection :: [Integer] -> [Integer] -> [Integer]
    > intersection []     bs = []
    > intersection (a:as) bs = if a `elem` bs then a:(intersection as bs) else intersection as bs

    ghci> intersection [1,2,3] [2,3,4]
    [2,3]

To find the union, we add all those elements that are in bs but not as to a.

    > union        :: [Integer] -> [Integer] -> [Integer]
    > union as [] = sort as
    > union as (b:bs) = if b `elem` as then union as bs else union (b:as) bs
     
    ghci> union [1,2,3] [2,3,4]
    [1,2,3,4]

To find the difference, we find the intersection of as and bs, then only return the elements of as 
that aren't in the intersection.

    > difference :: [Integer] -> [Integer] -> [Integer]
    > difference as bs = sort (difference2 as (intersection as bs))

    > difference2 :: [Integer] -> [Integer] -> [Integer]
    > difference2 []     bs = []
    > difference2 (a:as) bs = if a `elem` bs then difference2 as bs else a:(difference2 as bs)

    ghci> difference [1,2,3] [2,3,4]
    [1]
    
The disjunction is the difference of the union and intersection.

    > disjunction :: [Integer] -> [Integer] -> [Integer]
    > disjunction as bs = difference (union as bs) (intersection as bs)

    ghci> disjunction [1,2,3] [2,3,4]
    [1,4]
    
All of this assumes your set has no repeated elements. If you aren't sure, you can use this.

    > removeRepeats :: [Integer] -> [Integer]
    > removeRepeats [] = []
    > removeRepeats (a:as) = if a `elem` as then removeRepeats as else a : removeRepeats as

    ghci> removeRepeats [1,1,1,2,2,3,4,5,3,5]
    [1,2,4,3,5]

These functions will also be useful later.

    > len :: [Integer] -> Integer
    > len []     = 0
    > len (a:as) = 1+len as

    ghci> len [1,2,3,4,5]
    5

    > list1ton :: Integer -> [Integer]
    > list1ton n | n<1 = []
    > list1ton n = n:(list1ton (n-1))
    
    ghci> list1ton 10
    [10,9,8,7,6,5,4,3,2,1]
    
    > lcg :: Integer -> Integer -> [Integer]
    > lcg _ 1  = [1]
    > lcg r i = do 
    >   let n = inst r 
    >   (((n `mod` i)+1) : (lcg n (i-1)))

    > inst :: Integer -> Integer
    > inst r = ((5397*r+7901) `mod` 65536) 
    
It is now time for the unary operation, permutation of a set. The seed lets us get our array of random
numbers from the linear congruential generator.

    > permute :: Integer -> Integer -> [Integer]
    > permute seed n = do 
    >   let as = list1ton n
    >   let bs = lcg2 seed n
    >   permute2 as bs []

    > permute2 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    > permute2 as [] cs = cs 
    > permute2 as (b:bs) cs = do
    >   let n          = len as
    >   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
    >   permute2 (ds++es) bs (e:cs) 
    
    ghci> permute 65273853 9
    [9,2,8,1,5,4,7,6,3]
    ghci> permute 65273 9   
    [8,5,7,4,9,6,2,1,3]
    ghci> permute 653 9  
    [6,4,8,2,3,7,1,5,9]
