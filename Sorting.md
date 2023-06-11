I wrote these at night, well off the clock.


 - Merge Sort

       > mergeSort :: [Integer] -> [Integer]
       > mergeSort      [] = []
       > mergeSort     [x] = [x]
       > mergeSort   [x,y] = case (x>y) of
       >      False -> [x,y]
       >      True  -> [y,x]  
       > mergeSort list    = collate (mergeSort a) (mergeSort b)
       >      where (a, b) = splitAt ((length(list)) `div` 2) list
       
If the list has three or more elements, we split it, sort both, then collate. Haskell integer division
is such that, if the list has an odd number of elements, the first will be larger.

       > collate :: [Integer] -> [Integer] -> [Integer]
       > collate        as []  = as
       > collate (a:as) (b:bs) = if (a>b) then b:collate (a:as) bs 
       >                                  else a:collate as (b:bs)
    
These two lists are sorted, so either a or b will be our smallest element. So, we will collate once 
for each list element. 


 - Quick Sort

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
        
We pivot about the middle element, calling a new function that will cleave the list into two: one less,
one greater, and put the two into that empty list we give it. We then call quickSort again, on both 
halves, ordering them correctly. 
        
        > pivotAbout :: Integer -> [Integer] -> [[Integer]] -> [[Integer]]
        > pivotAbout n []     [a,b]  = [a,b]
        > pivotAbout n [x]    [a,b]  = if (x<n) 
        >                          then [x:a,b] 
        >                          else [a,x:b]
        > pivotAbout n (x:xs) [a,b]  = if (x<n) 
        >                          then pivotAbout n xs [x:a,b] 
        >                          else pivotAbout n xs [a,x:b]
       
These two algorithms are both n(log(n))ish, as they both work by splitting the initial lists in two as 
many times as necessary. I used the below function to test them. 
        
        
        > list1ton :: Integer -> [Integer]
        > list1ton n | n<1 = []
        > list1ton n = n:(list1ton (n-1))
