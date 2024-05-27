A Haskell implementation of Continued Fraction Arithmetic.
    
    {-# OPTIONS_GHC -Wall #-}
    
    import qualified Data.Ratio
    
    (//), (%) :: Integer -> Integer -> Integer
    (//) = quot
    (%) = rem
    
    
    -- REAL NUMBER APPROXIMATIONS
    
    
    newtype Q = Q (Integer, Integer)
      deriving Show
    newtype CFrac = CFrac [Integer]
      deriving Show
    newtype GCFrac = GCFrac [(Integer, Integer)]
      deriving Show
    
    
    see :: Q -> Double
    see (Q(a, b)) = (realToFrac a) / (realToFrac b)
    
    -- ghci> see $ Q(4,7)
    -- 0.5714285714285714
    
    instance Num Q where
      (+) (Q(j,k)) (Q(m,n)) =
        let (a,b) = (j*n + m*k, n*k) in
        let g = gcd a b in Q(a//g, b//g)
      -- 
      (-) (Q(j,k)) (Q(m,n)) = (+) (Q(j,k)) (Q(-m,n))
      (*) (Q(j,k)) (Q(m,n)) =
        let (a,b) = (j*m, n*k) in
        let g = gcd a b in Q(a//g, b//g)
      negate (Q(j,k)) = Q(-j,k)
      abs    (Q(j,k)) = Q(abs j, abs k)
      signum (Q(j,k)) = Q((signum j) * (signum k), 1)
      fromInteger j = Q(j, 1)
    
    -- ghci> Q(1,3) + Q(1,6)
    -- Q (1,2)
    -- ghci> Q(1,3) - Q(1,6)
    -- Q (1,6)
    -- ghci> Q(1,3) * Q(1,6)
    -- Q (1,18)
    -- ghci> Q(1,3) / Q(1,6)
    -- Q (2,1)
    -- ghci> negate $ Q(1,3)
    -- Q (-1,3)
    -- ghci> abs $ Q(1,3)
    -- Q (1,3)
    -- ghci> signum $ Q(1,3)
    -- Q (1,1)
    
    instance Fractional Q where
      (/) a (Q(c,d)) = (*) a (Q(d,c))
      recip (Q(a,b)) = Q(b,a)
      fromRational r = Q(Data.Ratio.numerator r, Data.Ratio.denominator r)
    
    instance Eq Q where
      (==) (Q(x,y)) (Q(m,n)) = (==) (m*y) (n*x)
    
    instance Ord Q where
      compare (Q(a,b)) (Q(c,d)) = compare (a*d) (b*c) 
    
    
    divide, modulo :: Q -> Q -> Q
    divide (Q(j,k)) (Q(m,n)) = (*) (Q(j,k)) (Q(n,m))
    modulo (Q(a,b)) (Q(c,d)) = (Q(a,b)) - (Q(c*((a*d) // (b*c)), d))
    
    -- Q has some flaws. Irrational numbers cannot be represented, and sometimes you want less
    -- precision. Ǝ a method allowing both.
    
    
    -- A continued fraction goes like
    
    --    1 / (2 + (1 / (3  + (1 / 4))))
    -- =  1 / (2 + (1 / (13 / 4))))
    -- =  1 / (2 + (4 / 13))
    -- =  1 / (30 / 13)
    -- = 13 / 30
    
    -- Numerators are always 1. The empty list is equivalent to
    -- dividing by zero, as both are not sensible.
    
    q2cf :: Q -> CFrac
    q2cf (Q(_, 0)) = CFrac []
    q2cf (Q(a, b)) = let CFrac xs = q2cf (Q(b, a % b)) in CFrac ((a // b) : xs)
    
    cf2q :: CFrac -> Q
    cf2q (CFrac []) = Q (1, 0)
    cf2q (CFrac (x:xs)) = let Q (a, b) = cf2q (CFrac xs) in Q (a*x+b, a)
    
    -- For infinite lists.
    cf2q_ :: CFrac -> Integer -> Q
    cf2q_ _      0 = Q (1, 0)
    cf2q_ (CFrac[]) _ = Q (1,0)
    cf2q_ (CFrac(x:xs)) n = let Q (a, b) = cf2q_ (CFrac xs) (n-1) in Q (a*x+b, a)
    
    -- ghci> cf2q_ (CFrac$ccycle [1]) 5 -> (8,5)
    -- ghci> cf2q_ (CFrac$ccycle [1]) 6 -> (13,8)
    -- ghci> cf2q_ (CFrac$ccycle [1]) 7 -> (21,13)
    -- ghci> cf2q_ (CFrac$ccycle [1]) 8 -> (34,21)
    
    eCF :: CFrac
    eCF = CFrac (2 : e' 0)
      where e' k = 1 : k+2 : 1 : e'(k+2)
    
    -- ghci> mapM (print . see . cf2q_ eCF) [1..10]
    -- 2.0
    -- 3.0                      Note it oscillates between
    -- 2.6666666666666665       below and above the target
    -- 2.75                     value (2.71828)
    -- 2.7142857142857144
    -- 2.71875
    -- 2.717948717948718
    -- 2.7183098591549295
    -- 2.718279569892473
    -- 2.718283582089552
    
    negateCF :: CFrac -> CFrac
    negateCF (CFrac [])       = CFrac []
    negateCF (CFrac [x])      = CFrac [-x]
    negateCF (CFrac (x:y:zs)) = clean $ CFrac (-x-1 : 1 : y-1 : zs)
    
    clean :: CFrac -> CFrac
    clean = q2cf . cf2q
    
    -- This function rewrites numbers so that they do not contain
    -- negatives or zeroes after the first number, and don't end 
    -- with a 1. In this format, there is exactly one way to write 
    -- every number.
    -- ghci> clean $ CFrac [2, 3, 0, 16, -5, 8] -> [2,18,1,3,1,7]
    -- ghci> clean $ CFrac [1,0]     -> []
    -- ghci> clean $ CFrac [1,0,1]   -> [2]
    -- ghci> clean $ CFrac [0,1,1,1] -> [0,1,2]
    
    
    
    
    -- ARITHMETIC
    
    -- (Matrix Operations)
    
    -- We need to iterate tensor multiplication to the desired
    -- degree of precision.
    
    
    -- HOMOGRAPHIC FUNCTION IS
    --
    --    ax + b
    --    ------
    --    cx + d
    -- We can encode this as a matrix.
    
    
    unCF :: CFrac -> [Integer]
    unCF (CFrac l) = l
    
    type Mtrx =  (Integer,Integer,Integer,Integer)
    type Tnsr = (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer)
    
    
    homo :: Mtrx -> [Integer] -> [Integer]
    homo (a,_,c,_) []      = unCF $ q2cf (Q(a,c))
    homo (0,b,0,d) _       = unCF $ q2cf (Q(b,d))
    homo (a,b,c,d) cf | c>0 && d>0 && a//c == b//d = (a//c) : homo (c,d,a%c,b%d) cf
    homo (a,b,c,d) (t:cf) = homo (t*a+b,a,t*c+d,c) cf
    
    -- ghci> homo (0,1,1,0) [0,1,4,3] <- reciprocal finder
    -- [1,4,3]
    -- ghci> homo (5,0,0,1) [0,1,1,2] <- homo (x,0,0,1) multiplies by x.
    -- [3]
    
    
    
    
    bihomo :: Tnsr -> [Integer] -> [Integer] -> [Integer]
    bihomo (a,b,_,_,e,f,_,_) [] ys  = homo (a,b,e,f) ys
    bihomo (a,_,c,_,e,_,g,_) xs []  = homo (a,c,e,g) xs
    --bihomo (0,0,c,0,0,_,g,h) _  ys  = homo (c,0,g,h) ys <- THIS WAS CAUSING IT
    --bihomo (0,b,0,d,0,0,_,h) xs _   = homo (b,d,0,h) xs 
    bihomo (a,b,c,d,e,f,g,h) xs ys | (abs (e*f*g*h)) > 0 && 
      (and$map (==(a//e)) [b//f,c//g,d//h]) =  
        (a//e) : bihomo (e,f,g,h,a%e,b%f,c%g,d%h) xs ys
    bihomo (a,b,c,d,e,f,g,h) (x:xs) ys | (f*g*h==0) || (abs$Q(b,f) - Q(d,h)) > (abs$Q(c,g) - Q(d,h)) =
         bihomo (a*x+c,b*x+d,a,b,e*x+g,f*x+h,e,f) xs ys
    bihomo (a,b,c,d,e,f,g,h) xs (y:ys) = bihomo (a*y+b,a,c*y+d,c,e*y+f,e,g*y+h,g) xs ys
    
    tAdd, tMul :: Tnsr
    tAdd = (0,  1, 1,  0, 0,  0, 0,  1)
    tMul = (1,  0, 0,  0, 0,  0, 0,  1)
    
    
    cfAdd, cfSub, cfMul :: CFrac -> CFrac -> CFrac
    cfAdd (CFrac x) (CFrac y) = CFrac (bihomo tAdd x y)
    cfMul (CFrac x) (CFrac y) = CFrac (bihomo tMul x y)
    cfSub (CFrac x) (CFrac y) = CFrac (bihomo tAdd x (unCF $ negate (CFrac y)))
    
    
    -- ghci> q2cf$(cf2q$CFrac[1,9,1,2])*(cf2q$CFrac[4,2])
    -- CFrac [4,1,28]
    -- ghci> CFrac[1,9,1,2]*CFrac[4,2]
    -- CFrac [4,1,28]
    
    -- ghci> q2cf$(cf2q$CFrac[1,9,1,2])*(cf2q$CFrac[4,2])
    -- CFrac [4,1,28]
    -- ghci> CFrac[1,9,1,2]*CFrac[4,2]
    -- CFrac [4,1,28]
    -- ghci> q2cf$(cf2q$CFrac[1,9,1,2])*(cf2q$CFrac[0,4,2])
    -- CFrac [0,4,12,1,4]
    -- ghci> q2cf$(cf2q$CFrac[0,1,9,1,2])*(cf2q$CFrac[4,2])
    -- CFrac [4,12,1,4]
    -- ghci> q2cf$(cf2q$CFrac[0,1,9,1,2])*(cf2q$CFrac[0,4,2])
    -- CFrac [0,4,1,28]
    -- ghci> CFrac[1,9,1,2]*CFrac[0,4,2]
    -- CFrac [0,4,12,1,4]
    -- ghci> CFrac[0,1,9,1,2]*CFrac[4,2]
    -- CFrac [4,12,1,4]
    -- ghci> CFrac[0,1,9,1,2]*CFrac[0,4,2]
    -- CFrac [0,4,1,28]
    -- ghci>
    
    
    
    instance Num CFrac where
      (+) = cfAdd
      (-) = cfSub
      (*) = cfMul
      negate = negateCF
      signum f = case clean f of
        CFrac []    -> CFrac [0]
        CFrac [0]   -> CFrac [0]
        CFrac (0:_) -> CFrac [1]
        CFrac (x:_) -> CFrac [signum x]
      abs f = f * (signum f)
      fromInteger j = CFrac [j]
    
    instance Eq CFrac where
      (==) x y = let (CFrac a, CFrac b) = (clean x, clean y) in a==b
    
    instance Ord CFrac where
      compare x y = case signum (x-y) of
        CFrac [0] -> EQ
        CFrac [1] -> GT
        _         -> LT
    
    instance Fractional CFrac where
      (/) x y = cfMul x (recip y)
      recip (CFrac xs) = clean (CFrac (0:xs))  
      fromRational r = q2cf $ Q (Data.Ratio.numerator r, Data.Ratio.denominator r)
    
    
    cfIntDiv :: CFrac -> CFrac -> CFrac
    cfIntDiv _ (CFrac [0]) = CFrac []
    cfIntDiv x y | y < x = CFrac [0]
    cfIntDiv x y = case x/y of
      CFrac [] -> CFrac []
      CFrac (i:_) -> CFrac [i]
    
    cfMod :: CFrac -> CFrac -> CFrac
    cfMod x y = x - ((cfIntDiv x y)*y)
    
    -- ghci> cf2q (CFrac [0,7])
    -- Q (1,7)
    -- ghci> cf2q (CFrac [0,1,3])
    -- Q (3,4)
    -- ghci> cfIntDiv (CFrac [0,1,3]) (CFrac [0,7])
    -- CFrac [5]
    -- ghci> cfMod (CFrac [0,1,3]) (CFrac [0,7])
    -- CFrac [0,28]
    
    
    -- ghci> q2cf (4,7)
    --   [0,1,1,3]
    -- ghci> q2cf (3,4)
    --   [0,1,3]
    -- ghci> (4,7) + (3,4)
    --   (37,28)
    -- ghci> q2cf (37,28)
    --   [1,3,9]
    -- ghci> [0,1,1,3] + [0,1,3]
    --   [1,3,9]
    
    
    
    -- circumference over diameter
    pi :: GCFrac
    pi = GCFrac $ (0,4):p 1 1
      where p a b = (b, a*a):p (a+1) (b+2)
    
    π :: Integer -> CFrac
    π = q2cf . (gcf2q_ Main.pi)
    
    -- ghci> mapM (print . see . cf2q . π) [1..10]
    -- 0.0
    -- 4.0
    -- 3.0
    -- 3.1666666666666665
    -- 3.1372549019607843
    -- 3.142342342342342
    -- 3.1414634146341465
    -- 3.1416149068322983
    -- 3.1415888250921244
    -- 3.1415933118799275
    
    exp :: Integer -> GCFrac
    exp x = GCFrac $ (0,1):(1,-x):e' x 1
      where e' x' i = (i+x',-i*x'):e' x' (i+1)
    
    exp' :: Integer -> Integer -> CFrac
    exp' x a = unGen $ GCFrac $ (0,1):(1,-x):e' x 1 a
      where e' _ _ 0 = []
            e' x' i a' = (i+x',-i*x'):e' x' (i+1) (a'-1)
    
    -- ghci> see $ cf2q $ exp' 2 10
    -- 7.388994708994709
    -- ghci> 2.71828 * 2.71828
    -- 7.3890461584
    
    ln :: Integer -> GCFrac
    ln xpo = let x = xpo-1 in GCFrac $ (0,x):l 1 x
      where l n x = (n - ((n-1)*x), n*n*x):l (n+1) x
    
    
    isqrt :: Integer -> Integer
    isqrt = rr 1
      where rr n nn = if (n*n) <= nn && (n*n+2*n+1) > nn then n
                      else rr ((n + nn//n) // 2) nn
    
    iroot :: Integer -> Integer -> Integer
    iroot = rr 1
      where rr n nn p = if (n^p) <= nn && ((n+1)^p) > nn
             then n
             else rr (((p-1)*n + nn//(n^(p-1))) // p) nn p
    
    sqrt :: Integer -> GCFrac
    sqrt i = let s = isqrt i in GCFrac $ (s, (i - s*s)) : cycle [(2*s, i - s*s)]
    
    sqrt' :: Integer -> Integer -> CFrac
    sqrt' i a = let s = isqrt i in unGen $ GCFrac $ (s, (i - s*s)) : take (fromInteger a) (cycle [(2*s, i - s*s)])
    
    -- ghci> sqrt' 5 10
    -- CFrac [2,4,4,4,4,4,4,4,4,4,4]
    -- ghci> sqrt' 18 10
    -- CFrac [4,4,8,4,8,4,8,4,8,4,8]
    -- ghci> sqrt' 97 10
    -- CFrac [9,1,5,1,1,1,1,1,1,5,1,18,1,5,1,1,1,1,1,1,4,1,11,1,2,2,1,1,4]
    -- ghci> sqrt' 97 20
    -- CFrac [9,1,5,1,1,1,1,1,1,5,1,18,1,5,1,1,1,1,1,1,5,1,18,1,5,1,1,1,1,1,1,5,1,18,1,5,1,2,2,2,1,12,1,2,33,2,47]
    
    
    
    
    pow :: Integer -> Integer -> Integer -> GCFrac
    pow z m n =
      let x = iroot z n in
      let y = z - (x^n) in
      let d = 2*x^n+y in
      GCFrac $ (x^m, (x^m)*2*m*y) : (n*d-m*y, -(n*n - m*m)*y*y) : pp n m d y 2
      where pp n' m' d y i = ((i+i-1)*n'*d, -(y*y*(i*i*n'*n' - m'*m'))) : pp n' m' d y (i+1)
    
    q2gcf :: Q -> GCFrac
    q2gcf = toGen . q2cf
    
    gcf2q :: GCFrac -> Q
    gcf2q (GCFrac []) = Q (1,0)
    gcf2q (GCFrac((a,b):xs)) = let Q (n,d) = gcf2q (GCFrac xs) in Q (d*b + n*a, n)
    
    gcf2q_ :: GCFrac -> Integer ->  Q
    gcf2q_ _ 0 = Q (1,0)
    gcf2q_ (GCFrac[]) _ = Q (0,1)
    gcf2q_ (GCFrac((a,b):xs)) i =
      let Q (n,d) = gcf2q_ (GCFrac xs) (i-1) in
      Q (d*b + n*a, n)
    
    unGen :: GCFrac -> CFrac
    unGen = q2cf . gcf2q
    
    toGen :: CFrac -> GCFrac
    toGen (CFrac l) = GCFrac (map ((,) 1) l)
    
    gcfAdd, gcfSub, gcfMul, gcfDiv :: GCFrac -> GCFrac -> GCFrac
    gcfAdd x y = toGen $ CFrac (bihomo tAdd (unCF $ unGen x) (unCF $ unGen y))
    gcfMul x y = toGen $ CFrac (bihomo tAdd (unCF $ unGen x) (unCF $ unGen y))
    gcfSub x y = toGen $ ((unGen x) - (unGen y))
    gcfDiv x y = toGen $ ((unGen x) / (unGen y))
    
    
    instance Num GCFrac where
      (+) = gcfAdd
      (-) = gcfSub
      (*) = gcfMul
      negate = q2gcf . negate . gcf2q
      signum = q2gcf . signum . gcf2q
      abs    = q2gcf . abs . gcf2q
      fromInteger j = GCFrac [(j,1)]
    
    instance Fractional GCFrac where
      (/) = gcfDiv
      recip = ((GCFrac [(1,1)])/)
      fromRational r = q2gcf (Q(Data.Ratio.numerator r, Data.Ratio.denominator r))
    
    
    
    
    qShow :: Q -> String
    qShow (Q(_,0)) = ""
    qShow (Q(0,_)) = ""
    qShow (Q(n,d)) = (show (n//d)) ++ "." ++ (qShow' (Q(10*(n - ((n//d)*d)),d)))
      where qShow' = (sans '.') . qShow
            sans _ [] = []
            sans c (x:xs) = if c==x then sans c xs else x : sans c xs
    
    
    -- SOURCES:
    --   https://hsinhaoyu.github.io/cont_frac/
    --   Gosper, "Continued Fraction Arithmetic"
    --   https://srossd.com/posts/2020-09-18-gosper-1/
    --   https://en.wikipedia.org/wiki/Continued_fraction
    --   https://r-knott.surrey.ac.uk/Fibonacci/cfINTRO.html
    --   https://web.archive.org/web/20130501130333/http://paul-mccarthy.us/Cfrac/CF_Arithmetic.htm
    
    
    
    
    
    
    
    
    
    
    
    
    
    -- SHREDS:
    
    
    -- instance Ord CFrac where
      -- (<=) x y = let c = y - x in if c==[] then True else head c >= 0
    
    -- power :: CFrac -> CFrac -> CFrac
    -- power xs ys = do
      -- let (a,b) = vnoc ys
    
    -- [(4,1),(1,3),(4,5),(9,7),(16,9)]
    
    -- toStd :: GCFrac
    
    
    -- toMtrx :: [Integer] -> Mtrx
    -- toMtrx [[a,b],[c,d]] = (a,b,c,d)
    -- toMtrx _ = (0,0,0,0)
    
    
    -- (#) :: Mtrx -> Mtrx -> Mtrx
    -- (m1,m2,m3,m4) # (n1,n2,n3,n4) = toMtrx [[sum $ zipWith (*) r c | c <- transpose n] | r <- m]
      -- where (m,n) = ([[m1,m2],[m3,m4]],[[n1,n2],[n3,n4]])
    -- Continued fractions work better as matrices: here's a new version of cf2q_ .
    -- Homographic transformations
    -- hm, h_ :: Integer -> Mtrx
    -- hm  a = (a,1,1,0)
    -- h_ b  = (0,1,1,b)
    
    -- cf_convergents_ :: Integer -> CFrac -> [Q]
    -- cf_convergents_ i (CFrac cf) = conv (1, 0, 0, 1) i cf
      -- where conv _ _ [] = []
            -- conv _ 0 _ = []
            -- conv m i' (x:xs) = let q@(a,_,b,_) = (m # (hm x)) in Q (a, b) : conv q (i'-1) xs
    
    -- ghci> map see $ cf_convergents_ 7 (1 : cycle [2]) ->
    --   [1.0,1.5,1.4,1.4166666666666667,1.4137931034482758,1.4142857142857144,1.4142011834319526]
    
    -- transpose :: Mtrx -> Mtrx
    -- transpose (xs:xss) = map (map (:) xs) (transpose xss)
    
    
    -- Homographic transformations
    -- h, h_ :: Integer -> Mtrx
    -- h  a = [[a,1],[1,0]]
    -- h_ b = [[0,1],[1,b]]
    
    -- atran :: Tnsr -> Mtrx -> Tnsr
    -- atran tsss mss =
      -- [[[sum $ zipWith (*) ts ms
        -- | ms <- mss]
        -- | ts <- tss]
        -- | tss <- tsss]
    
    -- btran :: Mtrx -> Tnsr -> Tnsr
    -- btran mss tsss =
      -- [[[sum $ zipWith (*) ts ms
        -- | ts  <- tss]
        -- | ms  <- mss]
        -- | tss <- tsss]
    
    -- apply_ab :: Tnsr -> Integer -> Bool -> Tnsr
    -- apply_ab t i True  = atran t (h  i)
    -- apply_ab t i False = btran (h_ i) t
    
    
    -- plus :: CFrac -> CFrac -> CFrac
    -- plus = cfAdd tAdd True
      -- where
            -- cfAdd t b (0:_) (0:_) = []
            -- cfAdd t True  (x:c1) (y:c2) = let t' = apply_ab t x True in let k = qrTensor t' in
                                            -- if isJust $ fst k
                                              -- then (getNum $ fst k) : cfAdd (snd k) False c1 (y:c2)
                                              -- else cfAdd (snd k) True  c1 (y:c2)
            -- cfAdd t False (x:c1) (y:c2) = let t' = apply_ab t y False in let k = qrTensor t' in
                                            -- if isJust $ fst k
                                              -- then (getNum $ fst k) : cfAdd (snd k) True (x:c1) c2
                                              -- else cfAdd (snd k) False  (x:c1) c2
            -- cfAdd t b [] [] = cfAdd t b []  []
            -- cfAdd t b [] c2 = cfAdd t b [0] c2
            -- cfAdd t b c1 [] = cfAdd t b c1 [0]
    
    
    
    
    -- A Homographic
    
    -- eucTens :: Tnsr -> [(Int, Tnsr)]
    -- eucTens [[[0,0],[0,0]], _] = []
    -- eucTens t@[[[aaa,aab],[aba,abb]],[[baa,bab],[bba,bbb]]] =
      -- let r = if baa > 0 && bab > 0 && bba > 0 && bbb > 0
        -- then [[aba//]]
        -- else (Nothing, [[[1, 0],[0, 1]],[[1, 0],[0, 1]]])
    
    -- qrTensor :: Tnsr -> (Maybe Integer, Tnsr)
    -- qrTensor t@[[[aaa,aab],[aba,abb]],[[baa,bab],[bba,bbb]]] =
      -- if all (>0) [aaa, aab, aba, abb] then
        -- let (w,x,y,z) = (baa//aaa, bab//aab, bba//aba, bbb//abb) in
        -- if w==x && y==z && x==z then (Just x, [[[baa,bab],[bba,bbb]],[[aaa-x*baa, aab-x*bab],[aba-x*bba, abb-x*bbb]]])
        -- else (Nothing, t)
      -- else (Nothing, t)
    
    
    
    -- cfArith :: CFrac -> CFrac -> Tnsr -> CFrac
    -- cfArith xs ys t False
