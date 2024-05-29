    import Data.List 
    import System.IO
    
    
    -- PS C:\Users\night> chcp 65001
    -- Active code page: 65001
    -- PS C:\Users\night> ghci
    -- GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
    -- ghci> :!chcp 65001
    -- Active code page: 65001
    -- ghci> :l Downloads/signpost.hs
    -- [1 of 2] Compiling Main             ( Downloads\signpost.hs, interpreted )
    -- Ok, one module loaded.
    -- ghci> signposts 4 62348632874 1
    --     |    |    |
    --  1→ |  ↘ |  → |  ↓
    -- ____|____|____|____
    --     |    |    |
    --   → |  ↓ |  ↙ |  ↙
    -- ____|____|____|____
    --     |    |    |
    --   ↗ |  ← |  ↓ |  ←
    -- ____|____|____|____
    --     |    |    |
    --   ↑ |  ↑ |  → | 16
    -- ____|____|____|____
    
    (%) = mod
    (//) = div
    
    rng :: Int -> Int -> [Int] 
    rng _ 0 = [] 
    rng o i = let n = r o in n : rng n (i-1)
      where r o = (1664525*o+1013904223) % (2 ^ 32)
    
    permute :: Int -> [a] -> [a]
    permute seed ns = let bs = rng seed (length ns) in p' bs ns []
      where p' [] _ cs = cs 
            p' (r:rs) bs cs = 
              let (ds, e:es) = splitAt (fromIntegral (r%(length bs))) bs
              in p' rs (ds++es) (e:cs) 
    
    
    queenTour :: Int -> Int -> [Int]
    queenTour n r = do 
      let ps = 1 : (permute r [2..n*n-1]) ++ [n*n]
      if valid n ps then inv ps else queenTour n (r+1)
    
    sign :: Int -> [Int] -> Int -> IO ()
    sign _ [] _ = return ()
    sign n ns i | n==i = return () 
    sign n ns i = do 
      putStrLn "    |    |    |"
      hSetEncoding stdout utf8
      hSetEncoding stdin utf8
      hSetEncoding stderr utf8
      putStrLn $ drop 2 $ prep (map (vector n ns) (take n (drop (n*i) ns)))
      if length ns == n then putStrLn "" else putStrLn "____|____|____|____"
      sign n ns (i+1)
    
    signposts :: Int -> Int -> Int -> IO () 
    signposts _ _ 0 = return ()
    signposts n r i = do 
      sign n (queenTour n r) 0
      putStrLn ""
      putStrLn ""
      signposts n ((r//7)*8) (i-1)
    
    valid :: Int -> [Int] -> Bool
    valid n (x:y:zs) = (elem (y-1) (connections n (x-1))) && valid n (y:zs)
    valid n _ = True
    
    -- ↖ 	↗ 	↘ 	↙   ← 	↑ 	→ 	↓ ↖ ↗ ↘ ↙
    
    vector :: Int -> [Int] -> Int -> String
    vector n g 1 = case idx 2 g 0 of 
      x|x<n -> "1→"
      x|(x%n)==0 -> "1↓"
      _ -> "1↘"
    vector n _ c | n*n == c = show $ n*n
    vector n g c = let (i,j) = ((idx c g 0), (idx (c+1) g 0)) in
      if i>j && (i%n) == (j%n)            then "↑" else
      if i<j && (i%n) == (j%n)            then "↓" else
      if i<j && (i//n) == (j//n)          then "→" else
      if i>j && (i//n) == (j//n)          then "←" else
      if (i//n) < (j//n) && (i%n) < (j%n) then "↘" else
      if (i//n) < (j//n) && (i%n) > (j%n) then "↙" else
      if (i//n) > (j//n) && (i%n) < (j%n) then "↗" else
      if (i//n) > (j//n) && (i%n) > (j%n) then "↖" else " "
    
    connections :: Int -> Int -> [Int]
    connections n c = 
      let (d,m) = divMod c n in
      sort $ filter (/=c) $
      [m + i*n     | i <- [0..n-1]]                            ++ -- V
      [i + d*n     | i <- [0..n-1]]                            ++ -- H
      [c - i*(n+1) | i <- [1..min m (c//(n+1))]]               ++ -- UL
      [c + i*(n+1) | i <- [1..min (n-m-1) ((n*n - c)//(n+1))]] ++ -- DR
      [c + i*(n-1) | i <- [1..min (n-d-1) m]]                  ++ -- DL
      [c - i*(n-1) | i <- [1..min (n-m-1) d]]                     -- UR
      
      
    prep :: [String] -> String
    prep [] = ""
    prep (x:xs) | (length x) > 1 = " | "  ++ x ++ prep xs
    prep (x:xs) = " |  "  ++ x ++ prep xs
    
    inv :: [Int] -> [Int]
    inv xs = [idx x xs 1 | x <- [1..16]]
    
    idx :: Eq a => a -> [a] -> Int -> Int
    idx x (y:ys) i = if x==y then i else idx x ys (i+1)
    idx _ [] _ = -1
