      -- ACAB

      -- INST CA : [NUMBER OF STATES]   :: Integer     
      --           [NUMBER OF DIMENS]   :: 1D OR 2D OR TORUS OR MOBIUSTRIP     
      -- 		  [NEIGHBORHOOD]       :: [Double] where each double represents the weight of a particular neighbor. each successive element spirals from Moore 					neighborhood like 

      -- 				234
      -- 				1X5  etc.
      -- 				876

      --      [INITIAL CONDITIONS] :: [Int]	begins with "default", then spirals from center for Particular Formations

      --      [TRANSITIONS]  :

      -- 		  A center state transitions according to a few different types of rules. We've got a decently general framework that runs both 110 and CGOL. requires a center and neighborhood length up top. Interacting w system looks like 

      -- 		  \\WOLFRAM//
      -- 		  INPUT CENTER
      -- 		  2
      -- 		  c=2
      -- 		  INPUT NBHD 
      -- 		  1
      -- 		  ERROR |n| < c
      -- 		  0 1 2
      -- 		  WOLFRAM NBHD 
      -- 		  OXO

      -- 		  INPUT SURVIVING [NBHD]

      -- 		  110 101 011 010 001

      -- 		  OR

      --      SUM=2 OR,TO SUM=3




      {-# LANGUAGE GADTs #-}
      {-# LANGUAGE FlexibleInstances    #-}
      {-# LANGUAGE ViewPatterns         #-}

      module ACAB where

      import           Parsing2
      import qualified Data.Map as M
      import Prelude
      import Control.Exception (ArithException)
      import Control.Monad.IO.Class (liftIO)
      import Text.Parsec
      import Text.Parsec.String
      import Data.Functor.Identity

      data Stt where
        S   ::                Char -> Stt  -- Particular
        R   ::                        Stt  -- Randomly Chosen on Startup
        deriving (Show, Eq)

      data Dimension where
        Default1D    :: Dimension
        Torus1D      :: Dimension
        Default2D    :: Dimension
        Torus2D      :: Dimension
        deriving (Show)

      type Size         =  Integer
      type States       =  Integer
      type Neighborhood = [Integer]
      type TransMtrx    = [[Integer]]
      type InitCond     = [Stt]
      type Iterances    =  Integer

      type Autom        =  Integer -> Size -> States -> Iterances -> Dimension -> 
                          Neighborhood -> TransMtrx -> InitCond 

      -- GOAL: TAKE THESE VARIABLES, RETURN A CELLULAR AUTOMATON AS SPECIFIED
      -- A Cell is an integer tuple.

      type Cell         = (Integer, Integer)

      -- A grid was a list of lists of cells. There can be up to 35 exterior lists: thus, cells can be in one of 36 states.

      type Grid         = [Stt]

      -- Initially, a grid was a [Cell]. Whichever cells were in the list were state 1, and the ones that weren't were state 0.
      -- To broaden this to more than two states, we initially went with a [[Cell]], where each of the (states-1) exterior lists 
      -- carried the cells in a particular state. This, ultimately, didn't allow for initial conditions to be specified by the user,
      -- which we only really figured out at 1 PM on May 9th. Thus, we changed it to a [State], rendering our convoluted "interpInit" 
      -- function obsolete.

      lexer :: TokenParser u
      lexer = makeTokenParser $ emptyDef    
        { reservedNames = ["1d", "1dt", "2d", "2dt", "];["] }

      parens     :: Parser a -> Parser a
      parens     = getParens lexer

      double     :: Parser Double
      double     = getFloat lexer

      natural    :: Parser Integer
      natural    = getInteger lexer

      whiteSpace :: Parser ()
      whiteSpace = getWhiteSpace lexer

      listbeg    :: Parser Double
      listbeg    = oneOf "[" *> double 

      listelem   :: Parser Double
      listelem   = oneOf "," *> double 

      listend    :: Parser Double
      listend    = oneOf "," *> double <* oneOf "]"

      --autom :: Parser Autom
      --autom = whiteSpace *> parseAutomAtom <* eof

      reserved   :: String -> Parser ()
      reserved   = getReserved lexer

      -- Our parsers get pretty out-there from a static-type perspective.
      -- Wrangling Haskell IO () has been a big little difficulty of this 
      -- project.


      parseDimen :: Parser Dimension
      parseDimen =    (Default1D    <$ reserved  "1d")
              <|>     (Default2D    <$ reserved  "2d")
              <|>     (Torus2D      <$ reserved "1dt")
              <|>     (Torus2D      <$ reserved "2dt")

      parseDimenIO :: IO Dimension
      parseDimenIO = do
        putStrLn "Input '1d', '2d', '1dt', or '2dt'"
        input <- getLine
        case runParserT parseDimen () "" input of
            Identity (Right dimn) -> return ( dimn)
            _ -> putStrLn ("Error: dimn") >> parseDimenIO

      parseNbhd :: Parser Neighborhood
      parseNbhd  = do 
        _ <- (oneOf "[") 
        nbhd <- (natural `sepBy` (oneOf ","))
        _ <- (oneOf "]")   
        return $ nbhd

      parseNbhdIO :: IO Neighborhood 
      parseNbhdIO = do
        putStrLn "Input neighborhood as a list of integers aligned to the Ulam Spiral." 
        input <- getLine
        case runParserT parseNbhd () "" input of
            Identity (Right nbhd) -> return ( nbhd)
            _ -> putStrLn ("Error: nbhd") >> parseNbhdIO

      parseTrnsMtrx :: Parser TransMtrx
      parseTrnsMtrx = do 
        _ <- (oneOf "[") 
        tsmx <- (natural `sepBy` (oneOf ",")) `sepBy` (reserved "];[")
        _ <- (oneOf "]")   
        return $ tsmx

      parseTrnsMtrxIO :: IO TransMtrx
      parseTrnsMtrxIO = do
        putStrLn "Input transition matrix as a list of lists of integers separated by commas separated by semicolons."
        putStrLn "like [0,1];[2,3,4];[5]"
        input <- getLine
        case runParserT parseTrnsMtrx () "" input of
            Identity (Right tsmx) -> return ( tsmx)
            _ -> putStrLn ("Error: tsmx") >> parseTrnsMtrxIO

      parseInit  :: Parser InitCond
      parseInit = do 
        _ <- (oneOf "[") 
        ic <- (parseInitElem `sepBy` (oneOf ","))
        _ <- (oneOf "]")   
        return $ ic

      parseInitElem :: Parser Stt
      parseInitElem =    R <$ (oneOf "?")
              <|>  (S '0') <$ (oneOf "0")   <|>  (S '1') <$ (oneOf "1")   <|>  (S '2') <$ (oneOf "2")   <|>  (S '3') <$ (oneOf "3")
              <|>  (S '4') <$ (oneOf "4")   <|>  (S '5') <$ (oneOf "5")   <|>  (S '6') <$ (oneOf "6")   <|>  (S '7') <$ (oneOf "7")
              <|>  (S '8') <$ (oneOf "8")   <|>  (S '9') <$ (oneOf "9")   <|>  (S 'Q') <$ (oneOf "Qq")  <|>  (S 'W') <$ (oneOf "Ww")
              <|>  (S 'E') <$ (oneOf "Ee")  <|>  (S 'R') <$ (oneOf "Rr")  <|>  (S 'T') <$ (oneOf "Tt")  <|>  (S 'Y') <$ (oneOf "Yy")
              <|>  (S 'U') <$ (oneOf "Uu")  <|>  (S 'I') <$ (oneOf "Ii")  <|>  (S 'O') <$ (oneOf "Oo")  <|>  (S 'P') <$ (oneOf "Pp")
              <|>  (S 'A') <$ (oneOf "Aa")  <|>  (S 'S') <$ (oneOf "Ss")  <|>  (S 'D') <$ (oneOf "Dd")  <|>  (S 'F') <$ (oneOf "Ff")
              <|>  (S 'G') <$ (oneOf "Gg")  <|>  (S 'H') <$ (oneOf "Hh")  <|>  (S 'J') <$ (oneOf "Jj")  <|>  (S 'K') <$ (oneOf "Kk")
              <|>  (S 'L') <$ (oneOf "Ll")  <|>  (S 'Z') <$ (oneOf "Zz")  <|>  (S 'X') <$ (oneOf "Xx")  <|>  (S 'C') <$ (oneOf "Cc")
              <|>  (S 'V') <$ (oneOf "Vv")  <|>  (S 'B') <$ (oneOf "Bb")  <|>  (S 'N') <$ (oneOf "Nn")  <|>  (S 'M') <$ (oneOf "Mm")

      parseInitIO :: IO InitCond
      parseInitIO = do
        putStrLn "Input initial conditions as a list of characters, separated by commas, enclosed by brackets." 
        putStrLn "They will be fitted to the smallest square possible." 
        input <- getLine
        case runParserT parseInit () "" input of
            Identity (Right o) -> return (o)
            _ -> putStrLn ("Error: init") >> parseInitIO


      main :: IO ()
      main = do
        putStrLn "INPUT LARGE RANDOM (INTEGER)"
        input0 <- getLine
        let seed = read input0 :: Integer in do
          putStrLn "INPUT SIZE       (INTEGER)"
          input1 <- getLine
          let size = read input1 :: Integer in do
            putStrLn "INPUT ITERATIONS (INTEGER)"
            putStrLn "NOTE YOU MAY GET LESS IF YOU REACH A STATIC STATE"
            input2 <- getLine
            let iter = read input2 :: Integer in do
              putStrLn "INPUT NUMBER OF STATES (INTEGER 2-36)"
              input3 <- getLine
              let stts = read input3 :: Integer in do
                putStrLn "------------"
                autom  <- initGame <$> parseInitIO
                                  <*> return seed 
                                  <*> return iter 
                                  <*> return size 
                                  <*> parseDimenIO
                                  <*> parseNbhdIO
                                  <*> parseTrnsMtrxIO
                                  <*> return stts
                autom   >> return ()
                
            
      -- Runs the game for a given number of iterations.


      initGame :: InitCond -> Integer -> Iterances -> Size -> Dimension -> Neighborhood -> TransMtrx -> States -> IO ()
      initGame ic rnd it sz dm nbhd tsmx s = do
        let rnds = lcg s rnd (sz*sz)
        let gr = makeInitGrid ic sz dm [] 0 rnds
        printGrid gr sz s rnds 0 dm
        if it == 0 then
          return ()
        else do
          let g' =  nextGrid gr [] sz dm nbhd tsmx s 0
          if  g' == gr then
            return ()
          else do
            runGame g' rnds (it-1) sz dm nbhd tsmx s

      lcg :: Integer -> Integer -> Integer -> [Integer]
      lcg s r i = do 
        let q = inst r 
        (modulus q s : lcg s q (i-1))

      inst :: Integer -> Integer
      inst r = (((5397*r)+7901) `mod` 65536) 

      makeInitGrid :: InitCond -> Size -> Dimension -> Grid -> Integer -> [Integer] -> Grid
      makeInitGrid []      sz dm gr i rnds =     if (i == (sz*sz)) then (gr) 
                                                                  else (makeInitGrid [] sz dm ((S '0'):gr) (i+1) rnds)
      makeInitGrid (mc:ic) sz dm gr i (r:rnds) = if (i == (sz*sz)) then (gr) 
                                                                  else (if (mc == R) 
                                                                  then (makeInitGrid ic sz dm ((S (head (integerToStr r))):gr) (i+1) rnds) 
                                                                  else (do let sq   = (ceiling ((fromIntegral (length ic)) ** 0.5)) 
                                                                            let c    = (sz - sq) `div` 2
                                                                            let x = (modulus i sz)
                                                                            let y = (i `div` sz)
                                                                            if  (((c < x)&&((c+sz) > x))&&((c < y)&&((c+sz) > y))) 
                                                                              then (makeInitGrid ic sz dm (mc:gr) (i+1) rnds)
                                                                              else (makeInitGrid (mc:ic) sz dm ((S '0'):gr) (i+1) rnds)))

      runGame :: Grid -> [Integer] -> Iterances -> Size -> Dimension -> Neighborhood -> TransMtrx -> States -> IO ()
      runGame gr rnds it sz dm nbhd tsmx s = do  
        printGrid gr sz s rnds 0 dm
        if it == 0 then
          return ()
        else do
          let g' =  nextGrid gr [] sz dm nbhd tsmx s 0
          if  g' == gr then
            return ()
          else do
            runGame g' (lcg s (last rnds) (sz*sz)) (it-1) sz dm nbhd tsmx s

      greaser :: Grid -> Size -> Grid
      greaser (g:gs) sz = if ((toInteger (length (g:gs))) == (sz*sz)) then g:gs 
                                                                else (if ((toInteger (length (g:gs))) > (sz*sz))
                                                                    then greaser gs sz
                                                                    else greaser ((S '0'):g:gs) sz)

      -- Prints the grid to the console.
      printGrid :: Grid -> Size -> States -> [Integer] -> Integer -> Dimension -> IO ()
      printGrid []     sz  s  rnds     i dm = do 
        let f       = (if ((modulus i sz) == (sz-1)) then "\n" else "")
        putStr (replicate (fromIntegral sz) '-' ++ "\n")
      printGrid (g':gs) sz s (r:rnds) i dm = do
        let g = (if (g' == R) then (S (head (integerToStr r))) else g')
        let (S stt) = g
        let f       = (if ((modulus i sz) == (sz-1)) then "\n" else "")
        putStr (stt:f)
        (if (f == "\n") then (case dm of
          Default1D -> putStr ("\n" ++ replicate (fromIntegral sz) '-')
          Torus1D   -> putStr ("\n" ++ replicate (fromIntegral sz) '-')
          _         -> printGrid gs sz s rnds (i+1) dm)
        else (printGrid gs sz s rnds (i+1) dm))


      -- determines the next state of the entire grid.

      -- 6:23 PM, May 9th. Shocked by how versatile the modulus / integer division combo has been in this project. Honestly,
      -- the thing I've learned this semester that's genuinely intrigued me the most is the concept of
      -- closing division within various finite and infinite fields, like \mathbf{N}, prime modular rings, and 
      -- n-dimensional stereographic projection.


      nextGrid :: Grid -> Grid -> Integer -> Dimension -> Neighborhood -> TransMtrx -> States -> Integer -> Grid
      nextGrid og ng sz dm nbhd tsmx s i = if (i == (sz*sz)) then ng else (do 
        let st = nextState og dm sz nbhd tsmx s (modulus i sz, i `div` sz)
        nextGrid og (st:ng) sz dm nbhd tsmx s (i+1))


      -- Determines the next state of a cell.

      nextState :: Grid -> Dimension -> Size -> Neighborhood -> TransMtrx -> States -> Cell -> Stt
      nextState g dm sz nbhd tsmx s c = do
        let w = fitToSpiral sz g dm nbhd [] 0 c 0
        checkTransMtrx tsmx w 1

      -- Determines the sum of neighbors*states of a given cell.

      neighbors :: Grid -> Dimension -> Neighborhood -> Cell -> Size -> Integer
      neighbors g dm nbhd (x, y) sz = fitToSpiral sz g dm nbhd [] 0 (x,y) 0 

      --Middleman
        
      fitToSpiral :: Size -> Grid -> Dimension -> Neighborhood -> [Stt] -> Integer -> Cell -> Integer -> Integer
      fitToSpiral _ _ _ [] _ _ _ sum = sum
      fitToSpiral sz g dm (c:nbhd) cs i (x, y) sum = if (c==0) then (fitToSpiral sz g dm nbhd cs (i+1) (x,y) sum) else (do 
        let (x', y') = (getCoord (x,y) i)
        let (S z)    = g !! (fromIntegral ((sz*(modulus y' sz))+(modulus x' sz)))
        let ww = (case dm of 
                    Default1D  -> (if ((x'>(-1))&&(x'<sz)&&(y' /= 0)) 
                      then           0
                      else           (c*(characterToInt z)))
                    Torus1D    -> (if (y' /= 0) 
                      then           0
                      else           (c*(characterToInt z)))
                    Default2D  -> (if ((y'>(-1))&&(y'<sz)&&(x'>(-1))&&(x'<sz)) 
                      then           (c*(characterToInt z)) 
                      else           0)
                    Torus2D    ->    (c*(characterToInt z)))
        (fitToSpiral sz g dm nbhd cs (i+1) (x,y) (sum+ww)))



      -- A296030, OEIS, "translated" from python with little regard for "A Richer Understanding..."
      -- input point (x,y) and neighborhood iterator n, get the cell n along the spiral from x,y


      getCoord :: Cell -> Integer -> Cell
      getCoord (x, y) n = do
        let k = (toInteger (ceiling ((((fromIntegral n) ** 0.5)-1)/2)))
        let t'=(2*k)+1
        let m=t'*t'
        let t=t'-1
        if ((n>(m-t))||(n==(m-t))) 
                    then (x+k+n-m,y-k) 
                    else (if ((n>(m-t-t))||(n>(m-t-t))) 
                            then (x+m-t-n-k,y+k) 
                            else (x+k,y+k+n+t-m-t))
        

      -- Determines the neighborhood-state-sum for a given cell. We abandoned the wolfram codes, ultimately, because 
      -- (even with the underscores) we couldn't figure out how to implement them in a way that was feasible to be used. 
      -- Generalized Conway notation is pretty good, though.



      -- callGetStateABunch :: Grid -> States -> Integer -> Neighborhood -> [Cell] -> Integer
      -- callGetStateABunch _ _ _ _ [] = 0
      -- callGetStateABunch g s x (l:nbhd) (n:ns) = ((getState' g s x n)*l)+(callGetStateABunch g s (x+1) nbhd ns) 


      -- Checks transition matrix to see which state the current nbhd-sum goes to.

      checkTransMtrx :: TransMtrx -> Integer -> Integer -> Stt
      checkTransMtrx [] _ _       = S '0'
      checkTransMtrx (a:tsmx) s x = if (s `elem` a) then (S (head (integerToStr x))) else (checkTransMtrx tsmx s (x+1))







      -- this is just `elem` but worse you airheaded irish hick

      contains :: Eq a => [a] -> a -> Bool 
      contains [] y     = False
      contains (x:xs) y = case (x==y) of
        True   -> True
        False  -> contains xs y




      -- makeAGrid :: Integer -> Grid -> Dimension -> Size -> Neighborhood -> TransMtrx ->  States -> Grid
      -- makeAGrid i (g':gs) dm sz nbhd tsmx s = if (i == (sz*sz)) then (g':gs) else do
      --   let (S a) = g'
      --   let (as, b:bs) = splitAt (fromIntegral (characterToInt a)) g
      --   makeAGrid dg (as ++ ((c:b):bs)) dm sz nbhd tsmx s


      -- interpInit :: States -> InitCond -> Integer -> Size -> Integer -> Grid -> Grid
      -- interpInit st ic sd sz it g = do 
      --   let sq = (ceiling ((fromIntegral (length ic)) ** 0.5)) 
      --   let c = (sz - sq) `div` 2
      --   let rnds = lcg st sd (sq*sq)
      --   goThroughThe ic rnds g sz sq (c, c) 

      -- https://www.youtube.com/watch?v=ig6F67VIn-k

      -- goThroughThe :: InitCond -> [Integer] -> Grid -> Size -> Integer -> Cell -> Grid
      -- goThroughThe []         _    g _  _   _     = g
      -- goThroughThe ((S s):ic) rnds g sz squ (x,y) = do 
      --   let state      = (characterToInt s)
      --   let b          = (g !!   (fromIntegral state))
      --   let (as, bs')  = splitAt (fromIntegral state) g
      --   let (_:bs)     = if (length bs > 0) then bs' else [(0,0)]:[[]]
      --   let z          = (sz - squ) `div` 2
      --   goThroughThe ic rnds (as ++ (b:bs)) sz squ   (if (x == (z+squ-1)) 
      --                                               then (                  if (y ==z+squ-1)
      --                                                                     then (z,z) 
      --                                                                     else (z,y+1) )
      --                                               else (x+1,y))
      -- goThroughThe (R:ic) (r:rnds) g sz squ (x,y) = do
      --   let b          = (g !! (fromIntegral r))
      --   let (as, bs')  = splitAt (fromIntegral r) g
      --   let (_:bs)     = if (length bs > 0) then bs' else [(0,0)]:[[]]
      --   let z          = (sz - squ) `div` 2
      --   goThroughThe ic rnds (as ++ (b:bs)) sz squ   (if (x == (z+squ-1)) 
      --                                               then (                  if (y ==z+squ-1)
      --                                                                     then (z,z) 
      --                                                                     else (z,y+1) )
      --                                               else (x+1,y))


        






      -- printCell :: Grid -> Size -> States -> Cell -> IO ()
      -- printCell (g':gs) sz s c (r:rnds)= do
      --   putStrLn "360"
      --   if (g' == R) then (let g = (S (head (integerToStr r)))) else (g = g')  
      --   let (S stt) = g
      --   let f       = (if ((snd c) == (sz-1)) then "\n" else "")
      --   putStr (stt ++ f)



      -- getState' :: Grid -> States -> Integer -> Cell -> Integer
      -- getState' []     _ _ _ = 0
      -- getState' (g:gs) s x c = if (x==s) then 0 else (if (c `elem` g) then x else (getState' gs s (modulus (x+1) s) c))



      integerToStr :: Integer -> String
      integerToStr  0 = "0"  
      integerToStr  1 = "1"  
      integerToStr  2 = "2"  
      integerToStr  3 = "3"  
      integerToStr  4 = "4"  
      integerToStr  5 = "5"  
      integerToStr  6 = "6"  
      integerToStr  7 = "7"  
      integerToStr  8 = "8"  
      integerToStr  9 = "9"  
      integerToStr 10 = "A"  
      integerToStr 11 = "B"  
      integerToStr 12 = "C"  
      integerToStr 13 = "D"  
      integerToStr 14 = "E"  
      integerToStr 15 = "F"  
      integerToStr 16 = "G"  
      integerToStr 17 = "H"  
      integerToStr 18 = "I"  
      integerToStr 19 = "J"  
      integerToStr 20 = "K"  
      integerToStr 21 = "L"  
      integerToStr 22 = "M"  
      integerToStr 23 = "N"  
      integerToStr 24 = "O"  
      integerToStr 25 = "P"  
      integerToStr 26 = "Q"  
      integerToStr 27 = "R"  
      integerToStr 28 = "S"  
      integerToStr 29 = "T"  
      integerToStr 30 = "U"  
      integerToStr 31 = "V"  
      integerToStr 32 = "W"  
      integerToStr 33 = "X"  
      integerToStr 34 = "Y"  
      integerToStr 35 = "Z"  
      integerToStr x  = integerToStr (modulus x 35)  

      characterToInt :: Char -> Integer
      characterToInt '0' =  0
      characterToInt '1' =  1
      characterToInt '2' =  2
      characterToInt '3' =  3
      characterToInt '4' =  4
      characterToInt '5' =  5
      characterToInt '6' =  6
      characterToInt '7' =  7
      characterToInt '8' =  8
      characterToInt '9' =  9
      characterToInt 'A' = 10
      characterToInt 'B' = 11
      characterToInt 'C' = 12
      characterToInt 'D' = 13
      characterToInt 'E' = 14
      characterToInt 'F' = 15
      characterToInt 'G' = 16
      characterToInt 'H' = 17
      characterToInt 'I' = 18
      characterToInt 'J' = 19
      characterToInt 'K' = 20
      characterToInt 'L' = 21
      characterToInt 'M' = 22
      characterToInt 'N' = 23
      characterToInt 'O' = 24
      characterToInt 'P' = 25
      characterToInt 'Q' = 26
      characterToInt 'R' = 27
      characterToInt 'S' = 28
      characterToInt 'T' = 29
      characterToInt 'U' = 30
      characterToInt 'V' = 31
      characterToInt 'W' = 32
      characterToInt 'X' = 33
      characterToInt 'Y' = 34
      characterToInt 'Z' = 35
      characterToInt x   = 0


      -- glider :: Grid
      -- glider = [[(0,1),(1,2),(2,0),(2,1),(2,2)]]


      modulus :: Integer -> Integer -> Integer
      modulus a b
        | ((-1) < a) && (a < b) =                 a
        |  0 > b                = (-1) * modulus  a      ((- 1) * b)
        |  a < b                =        modulus (a + b)          b
        |  otherwise            =        modulus (a - b)          b











        -- putStrLn "INPUT INITCOND [STATE]"
        -- input4 <- getLine
        -- let init = parseInit 
        -- putStrLn "INPUT NBHD ([INTEGER])"
        -- input4 <- getLine
        -- let nbhd = parseNbhd
        -- putStrLn "INPUT DIMEN"
        -- input5 <- getLine
        -- let dmns = 5
        -- putStrLn "INPUT TRANSITION MATRIX"
        -- input7 <- getLine
        -- let tsmx = (read input7 :: Integer)
        
        -- putStrLn "------------"
        -- runGame [(0,0)] iter size Torus2D 


      toDimen :: String -> Dimension
      toDimen "1d " =   Default1D
      toDimen "2d " =   Default2D
      toDimen "1dt" =   Torus1D
      toDimen "2dt" =   Torus2D
      toDimen  ___  =   Torus2D


        -- putStrLn "INPUT CONWAY TRANSITION MATRIX"
        -- input4 <- getLine
        -- let transmtrx = (read input4 :: ([Integer],[Integer]))  


      toGrid :: [Stt] -> Integer -> Integer -> Grid -> Grid
      toGrid []             _ _    g = g
      toGrid ((S x):states) n size g = undefined


        -- putStrLn "INPUT STATES (INTEGER)"
        -- input2 <- getLine
        -- let states = (read input2 :: Integer)
        -- putStrLn "INPUT DIMEN (1d, 2d, 1dt, or 2dt)"
        -- input3 <- getLine
        -- let states = (show input2)



      data InterpError where
        Dimen :: InterpError

      -- parseAutomAtom :: IO () -> Autom
      -- parseAutomAtom = undefined 


      -- do 
      --        putStrLn "enter amount of states"
      --        states <- natural
      --        putStrLn "enter dimension" 
      --        dimen  <- parseDimen
      --        putStrLn "enter size" 
      --        vandh  <- natural
      --        putStrLn "enter neighborhood" 
      --        vandh  <- natural

      type ACABFun = Integer -> Integer -> Integer -> Autom -> Int

      evalACAB :: String -> Either String ACABFun
      evalACAB s = undefined




