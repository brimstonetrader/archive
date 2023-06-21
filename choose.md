# CHOOSE

Below is the worst way to do this.

    > choose :: Integer -> Integer -> Integer
    > choose n k = if n==k || n<1 then 1 else (choose (n-1) (k-1))+(choose (n-1) k)

20 choose 10 took several seconds. This will not do.

    > choose2 :: Integer -> Integer -> Integer
    > choose2 n k = if n==k then [1] else (choose (n-1) (k-1)) + (choose2 (n-1) k)

Much better! Maybe the factorial could help though.

    > factorial :: Integer -> Integer
    > factorial 1 = 1
    > factorial n = n*(factorial (n-1))
    
    > choose3 :: Integer -> Integer -> Integer
    > choose3 n k = ((factorial n) `div` (factorial (n-k))) `div` (factorial k)

Faster still, but there's some unnecessary calculations in there. A "fall" function could help.

    > fall :: Integer -> Integer -> Integer
    > fall n k = if n==k then 1 else n*(fall (n-1) k)
    
    > choose :: Integer -> Integer -> Integer
    > choose n k = (fall n k) `div` (factorial (n-k))
