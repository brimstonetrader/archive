            ------------

            > {-# LANGUAGE GADTs #-}
            > {-# OPTIONS_GHC -Wall #-}
            >
            > module Calc where
            >
            > import           Parsing2
            > import qualified Data.Map as M
            > import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))

            > description :: String
            > description = unlines
            >   [ "WELCOME TO THE WIRE MATRIX"
            >   , "WE HAVE BEEN EXPECTING YOU"
            >   , "FLOATING POINT ARITHMETIC"
            >   , "COMBINATORICS OF N->K FOR NATURALS @ THE suth FUNCTION"
            >   , "WE DON'T TAKE KINDLY TO TRANSCENDENTALS"
            >   , "NEVERTHELESS WE HAVE NAUGHTY, ALGEBRAIC TRIG"
            >   , "FOR ACCESS TO THE FABLED CHART"
            >   , "GIVE ME A FUNCTION, :help, :suthhelp, or :quit."
            >   ]

            > helpMsg :: String
            > helpMsg = unlines
            >   [ "floating point values (https://www.youtube.com/watch?v=dQhj5RGtag0)"
            >   , "negation, standard arithmetic operators + - * / ^ ."
            >   , "e's about 2.7, I heard pi is less than 4."
            >   , "e takes a Natural input, which reflects the degree of precision you want."
            >   , "ex. e 5 = 1/0! + 1/1! + 1/2! + 1/3! + 1/4!"
            >   , "phi's the golden ratio. You can call fib(a)"
            >   , "for the a'th term of the sequence, and fib(a,b,c)"
            >   , "for the c'th term of a fibonacci-style sequence."
            >   , "with starting terms a and b."
            >   , "c is the speed of light in meters per second."
            >   , "to find a square root, raise to power of 1/2."
            >   , "sin(x) and cos(x) and tan(x) are sort of right."
            >   , "if you believe bhaskara"
            >   ]

            > suthHelpMsg :: String
            > suthHelpMsg = unlines
            >   [ "Look if you really want to know"
            >   , "about The Chart..."
            >   , "click this Overleaf link"
            >   , "https://www.overleaf.com/read/nbzrcyjnnxmk"
            >   , "and if you want to know how these functions"
            >   , "are implemented, email me"
            >   ]

            This is the main function that is called by `CalcREPL` to evaluate
            user input.

            > calc :: String -> String
            > calc s = case parse arith s of
            >   Left pErr  -> show pErr
            >   Right e    ->
            >     case interpArith M.empty e of
            >       Left iErr -> show (showInterpError iErr)
            >       Right v   -> (displayArith M.empty e) ++ " = " ++ (show v)

            // This parses the string, returning an error if need be, then interprets the 
            // arith, again, throwing an error if need be.

            > displayArith :: Env -> Arith -> String
            > displayArith _ (Lit (Left x))                = (show x)
            > displayArith _ (Lit (Right x))               = (show x)
            > displayArith e (Neg a)                    = "-" ++ (displayArith e a)
            > displayArith _ (E a)                      = "e{" ++ (show a) ++ "}"
            > displayArith e (FibGen a1 a2 n)            = "Fib(" ++ (displayArith e a1) ++ " , " ++ (displayArith e a2) ++ " , " ++ (show n) ++ ")" 
            > displayArith e (Bin Plus a1 a2)           = "(" ++ (displayArith e a1) ++ " + " ++ (displayArith e a2) ++ ")"
            > displayArith e (Bin Minus a1 a2)          = "(" ++ (displayArith e a1) ++ " - " ++ (displayArith e a2) ++ ")"
            > displayArith e (Bin Times a1 a2)          = "(" ++ (displayArith e a1) ++ " * " ++ (displayArith e a2) ++ ")"
            > displayArith e (Bin Div a1 a2)            = "(" ++ (displayArith e a1) ++ " / " ++ (displayArith e a2) ++ ")"
            > displayArith e (Bin Raise a1 a2)          = "(" ++ (displayArith e a1) ++ " ^ " ++ (displayArith e a2) ++ ")"
            > displayArith e (Bin Mod a1 a2)            = "(" ++ (displayArith e a1) ++ " % " ++ (displayArith e a2) ++ ")"
            > displayArith _ (Var x)                    = x
            > displayArith e (Let x a1 a2)              = "Let " ++ x ++ " = " ++ (displayArith e a1) ++ " in " ++ (displayArith e a2)
            > displayArith e (Trig Sin a)               = "sin(" ++ (displayArith e a) ++ ")"
            > displayArith e (Trig Cos a)               = "cos(" ++ (displayArith e a) ++ ")"
            > displayArith e (Trig Tan a)               = "tan(" ++ (displayArith e a) ++ ")"
            > displayArith e (Suth dist func ord n k)   = 
            >       "suth(" ++ (displayDistinction dist) ++ ", " ++ (displayFuncClass func) 
            >       ++ ", " ++ (if (ord == False) then ("") else ("o, ")) ++ (displayArith e n)
            >       ++ ", " ++(displayArith e k) ++ ")"
            > displayArith _ _ = ""


            // I have put parentheses around the operations, but other than that am just displaying the Arith
            // before it gets interpreted in addition to after.

            > displayDistinction :: Distinction -> String
            > displayDistinction DaD = "DaD"
            > displayDistinction IaD = "IaD"
            > displayDistinction DaI = "DaI"
            > displayDistinction IaI = "IaI"

            > displayFuncClass :: FuncClass -> String
            > displayFuncClass ANY = "ANY"
            > displayFuncClass SUR = "SUR"
            > displayFuncClass INJ = "INJ"
            > displayFuncClass BIJ = "BIJ"

            // boilerplate

            > data Arith where
            >   Lit       :: (Either Integer Double) -> Arith 
            >   Neg       ::  Arith       -> Arith
            >   Fact      ::  Arith       -> Arith
            >   Partition ::  Arith       -> Arith
            >   Bin       ::  Op          -> Arith       -> Arith   -> Arith
            >   Var       ::  String      -> Arith
            >   Let       ::  String      -> Arith       -> Arith   -> Arith
            >   E         ::  Integer     -> Arith
            >   Trig      ::  Pytho       -> Arith       -> Arith 
            >   FibGen    ::  Arith       -> Arith       -> Integer -> Arith
            >   Suth      ::  Distinction -> FuncClass   -> Bool    -> Arith -> Arith -> Arith
            >   deriving (Show)

            // e, sin, cos, tan, modulus, and fib are new. suth has a whole mess of other stuff going on underneath it.
            // we've also got support for pi, phi, and c. The level 3 boosters i'm going for are the first (wildcard)
            // and implementing Dr. Sutherland's chart. (The user can't directly compute sums or factorials, only suth functions).


            > data Distinction where
            >   DaD  :: Distinction
            >   IaD  :: Distinction
            >   DaI  :: Distinction
            >   IaI  :: Distinction
            >   deriving (Show, Eq) 
            >
            > data FuncClass where
            >   ANY  :: FuncClass
            >   SUR  :: FuncClass
            >   INJ  :: FuncClass
            >   BIJ  :: FuncClass
            >   deriving (Show, Eq)
            >
            > data Pytho where
            >   Sin :: Pytho
            >   Cos :: Pytho
            >   Tan :: Pytho
            >   deriving (Show, Eq)
            >
            > data Op where
            >   Plus   :: Op
            >   Minus  :: Op
            >   Times  :: Op
            >   Div    :: Op
            >   Raise  :: Op
            >   Mod    :: Op
            >   Stir   :: Op
            >   Fall   :: Op
            >   Choose :: Op
            >   Part   :: Op
            >   deriving (Show, Eq)

            > lexer :: TokenParser u
            > lexer = makeTokenParser $ emptyDef
            >   { reservedNames = ["let", "in", "sum", "from", "to", "e", "pi", "phi", "sin", "cos", "tan", "fib", 
            >                      "suth", "DaD", "DaI", "IaD", "IaI", "any", "sur", "inj", "bij", "o", "c"] }
            >
            > parens :: Parser a -> Parser a
            > parens     = getParens lexer
            >
            > reservedOp :: String -> Parser ()
            > reservedOp = getReservedOp lexer
            >
            > reserved :: String -> Parser ()
            > reserved   = getReserved lexer
            >
            > double :: Parser Double
            > double     = getFloat lexer
            >
            > natural :: Parser Integer
            > natural    = getInteger lexer
            >
            > naturalOrFloat :: Parser (Either Integer Double)
            > naturalOrFloat = getNaturalOrFloat lexer 
            >
            > negative :: Parser Arith
            > negative = ((oneOf "-") *> parseArithAtom)
            >
            > funcclass :: Parser FuncClass
            > funcclass = (ANY <$ reserved "any")
            >         <|> (INJ <$ reserved "inj")
            >         <|> (SUR <$ reserved "sur")
            >         <|> (BIJ <$ reserved "bij")
            >
            > distinction :: Parser Distinction
            > distinction = (DaD <$ reserved "DaD")
            >           <|> (DaI <$ reserved "DaI")
            >           <|> (IaD <$ reserved "IaD")
            >           <|> (IaI <$ reserved "IaI")
            >
            > whiteSpace :: Parser ()
            > whiteSpace = getWhiteSpace lexer
            >
            > identifier :: Parser String
            > identifier = getIdentifier lexer
            >
            > parseArithAtom :: Parser Arith
            > parseArithAtom =
            >       (Neg <$> negative) 
            >   <|>  Lit <$> naturalOrFloat
            >   <|>  Var <$> identifier
            >   <|>  parseLet
            >   <|>  Trig Sin <$> (reserved "sin"   *> parseArith)
            >   <|>  Trig Cos <$> (reserved "cos"   *> parseArith)
            >   <|>  Trig Tan <$> (reserved "tan"   *> parseArith)
            >   <|> (Bin Div (Bin Plus (Lit (Left 1)) (Bin Raise (Lit (Left 5)) (Lit (Right 0.5)))) (Lit (Left 2))) <$ (reserved "phi")
            >   <|> (Lit (Right 299792458)) <$ (reserved "c")
            >   <|> (Lit (Right 3.14159265358979)) <$ (reserved "pi")
            >   <|>  E <$> (reserved "e" *> natural)
            >   <|>  parseFib
            >   <|>  parseFibGen
            >   <|>  parseSuth
            >   <|>  parens parseArith
            > 
            > parseSuth :: Parser Arith
            > parseSuth = Suth 
            >   <$> ( reserved "suth" 
            >    *>   distinction)
            >   <*>   funcclass
            >   <*> ((True <$ (oneOf "o")) <|> (False <$ whiteSpace))
            >   <*>   parseArith
            >   <*>   parseArith
            >
            > parseFibGen :: Parser Arith
            > parseFibGen = FibGen
            >   <$> (reserved "fibgen"
            >    *> (Lit <$> naturalOrFloat))
            >   <*> (Lit <$> naturalOrFloat)
            >   <*> (natural)
            >
            > parseFib :: Parser Arith
            > parseFib = FibGen (Lit (Left 1)) (Lit (Left 1))
            >   <$> (reserved "fib" *> natural)
            >
            > parseLet :: Parser Arith
            > parseLet = Let
            >   <$> (reserved   "let" *> identifier)
            >   <*> (reservedOp "="   *> parseArith)
            >   <*> (reserved   "in"  *> parseArith)
            >
            > parseArith :: Parser Arith
            > parseArith = buildExpressionParser table parseArithAtom
            >   where
            >     table = [ 
            >               [ Infix  (Bin Raise <$ reservedOp "^") AssocRight],
            >               [ Prefix (Neg       <$ reservedOp "-")],
            >               [ Infix  (Bin Times <$ reservedOp "*") AssocLeft
            >               , Infix  (Bin Div   <$ reservedOp "/") AssocLeft
            >               , Infix  (Bin Mod   <$ reservedOp "%") AssocLeft
            >               ]
            >             , [ Infix  (Bin Plus  <$ reservedOp "+") AssocLeft
            >               , Infix  (Bin Minus <$ reservedOp "-") AssocLeft
            >               ]
            >             ]
            >
            > arith :: Parser Arith
            > arith = whiteSpace *> parseArith <* eof
            >
            > -- Interpreter
            >
            > type Env = M.Map String Double
            >
            > data InterpError where
            >   UnboundVar :: String -> InterpError
            >   DivByZero  :: InterpError
            >   Unnatural  :: InterpError
            >   YouFucked  :: InterpError
            >
            > showInterpError :: InterpError -> String
            > showInterpError (UnboundVar x) = "Unbound variable " ++ x
            > showInterpError DivByZero      = "Division by zero"
            > showInterpError Unnatural      = "You tried to run combinatorics on something real..."
            > showInterpError YouFucked      = "Don't do that!!"
            >
            > interpArith :: Env -> Arith -> Either InterpError Double
            > interpArith _ (Lit i)           = case i of
            >   (Left j)                      -> Right (realToFrac j)
            >   (Right j)                     -> Right j
            > interpArith e (Neg a)           = (*) <$> (Right (-1.0))   <*> interpArith e a
            > interpArith e (Bin Plus e1 e2)  = (+) <$> interpArith e e1 <*> interpArith e e2
            > interpArith e (Bin Minus e1 e2) = (-) <$> interpArith e e1 <*> interpArith e e2
            > interpArith e (Bin Times e1 e2) = (*) <$> interpArith e e1 <*> interpArith e e2
            > interpArith e (Bin Div e1 e2)   =
            >   interpArith e e2 >>= \v ->
            >   case v of
            >     0 -> Left DivByZero
            >     _ -> (/) <$> interpArith e e1 <*> Right v
            > interpArith e (Bin Mod e1 e2)   = (modulus) <$> interpArith e e1 <*> interpArith e e2
            > interpArith e (Bin Raise e1 e2) = (**) <$> interpArith e e1 <*> interpArith e e2
            > interpArith e (Var x)           =
            >   case M.lookup x e of
            >     Nothing -> Left $ UnboundVar x
            >     Just v  -> Right v
            > interpArith e (Let x e1 e2)     =
            >   interpArith e e1 >>= \v ->
            >   interpArith (M.insert x v e) e2
            > interpArith e (Trig Sin x) = (sine) <$> (interpArith e x)
            > interpArith e (Trig Cos x) = (cosine) <$>  (interpArith e x)
            > interpArith e (Trig Tan x) = (tangent) <$>  (interpArith e x)
            > interpArith _ (E n) = (Right (eTaylor n 0))
            > interpArith e (FibGen x y n) = (fibgen) <$> (interpArith e x) <*> (interpArith e y) <*> (Right n)
            > interpArith e (Suth dist fun o n k) = case ((suth dist fun o) <$> (suthChecker e n) <*> (suthChecker e k)) of
            >   Left x  -> Left x
            >   Right x -> interpArith e x
            > interpArith e x = case (nBinHelper e x) of
            >   Left er -> Left er
            >   Right a -> Right (realToFrac a)
            >
            >
            > sumHelper :: Env -> String -> Integer -> Integer -> Arith -> Arith
            > sumHelper e i beg end expr = if (beg == end) then (Let i (Lit (Left beg)) expr)  else
            >   (Bin Plus (Let i (Lit (Left beg)) expr) (sumHelper e i (beg+1) end expr))
            >
            > suthChecker :: Env -> Arith -> Either InterpError Integer
            > suthChecker e a = case (interpArith e a) of
            >   Right x      -> Right (round x)
            >   _            -> (Left Unnatural)
            >
            > nBinHelper :: Env -> Arith -> Either InterpError Integer
            > nBinHelper e (Fact n) = (factorial) <$> suthChecker e n  <*> Right 1
            > nBinHelper e (Partition n) = partition <$> suthChecker e n
            > nBinHelper e (Bin Choose n k) = choose <$> suthChecker e n <*> suthChecker e k
            > nBinHelper e (Bin Stir n k) = stir <$> suthChecker e n <*> suthChecker e k
            > nBinHelper e (Bin Fall n k) = fall <$> suthChecker e n <*> suthChecker e k
            > nBinHelper e (Bin Part n k) = part <$> suthChecker e n <*> suthChecker e k
            > nBinHelper _ _                 = Left YouFucked
            >
            > tangent :: Double -> Double
            > tangent x = (sine x) / (cosine x)
            >
            > cosine :: Double -> Double
            > cosine x = sine (x+(3.14159265359 / 2))
            >
            > bhaskara :: Double -> Double
            > bhaskara x = (16*x*(3.14159265359 - x))/((5*(3.14159265359 ** 2))-(4*x*(3.14159265359 - x)))
            >
            > sine :: Double -> Double 
            > sine 0 = 0
            > sine x = case ((modulus x 6.28318530718)>3.1415926535) of 
            >   False -> (truncate' (bhaskara (modulus x 6.28318530718)) 4)
            >   True  -> (truncate' ((-1) * bhaskara ((modulus x 6.28318530718)-3.1415926535)) 4)

            -- x : number you want rounded, n : number of decimal places you want...
            -- https://stackoverflow.com/questions/18723381/rounding-to-specific-number-of-digits-in-haskell

            > truncate' :: Double -> Int -> Double
            > truncate' x n = (x * (10**(realToFrac n))) / (10**(realToFrac n))
            >
            > modulus :: Double -> Double -> Double
            > modulus a b = case ((0<a) && (a<b)) of
            >    True  -> a
            >    False -> case (0>b) of
            >      True -> (-1 * (modulus a ((-1) * b)))
            >      False -> case (a < b) of
            >        True -> (modulus (a+b) b)
            >        False -> (modulus (a-b) b) 




            > suth :: Distinction -> FuncClass -> Bool -> Integer -> Integer -> Arith
            > suth DaD ANY False n k =  Lit (Left (n ^ k))
            > suth DaD SUR False n k = (Bin Times (Fact (Lit (Left n))) (Bin Stir (Lit (Left k)) (Lit (Left n))))
            > suth DaD INJ False n k = (Bin Fall (Lit (Left n)) (Lit (Left k)))
            > suth DaD BIJ False n _ =  Fact (Lit (Left n))
            > suth DaI ANY False n k = case (n<k) of 
            >   True  -> (sumHelper M.empty "i" 1 n (Bin Stir (Lit (Left k))(Var "i"))) 
            >   False -> (sumHelper M.empty "i" 0 n (Bin Stir (Lit (Left n)) (Var "i")))
            > suth DaI SUR False n k = (Bin Stir (Lit (Left k)) (Lit (Left n)))
            > suth DaI INJ False n k = case (n<k) of 
            >   True -> (Lit (Left 1)) 
            >   False -> (Lit (Left 0))
            > suth _ BIJ False n k   = case (n<k) of 
            >   True -> (Lit (Left 1)) 
            >   False -> (Lit (Left 0))
            > suth IaD ANY False n k = (Bin Choose (Lit (Left (n+k-1))) (Lit (Left k)))
            > suth IaD SUR False n k = (Bin Choose (Lit (Left (k-1))) (Lit (Left (n-1))))
            > suth IaD INJ False n k = (Bin Choose (Lit (Left n)) (Lit (Left k)))
            > suth IaI ANY False n k = case (n<k) of 
            >   True -> (sumHelper M.empty "i" 1 n (Bin Part (Lit (Left k)) (Var "i"))) 
            >   False -> (Partition (Lit (Left n)))
            > suth IaI SUR False n k = (Bin Part (Lit (Left k)) (Lit (Left n)))
            > suth IaI INJ False n k = case (n<k) of 
            >   True  -> (Lit (Left 1)) 
            >   False -> (Lit (Left 0))
            > suth DaD ANY True n k  = (Bin Fall (Lit (Left (n+k-1))) (Lit (Left k)))
            > suth DaD SUR True n k  = (Bin Choose (Lit (Left (k-1))) (Lit (Left (n-1))))
            > suth DaI ANY True n k  = (sumHelper M.empty "i" 1 n (Bin Times 
            >   (Bin Choose (Lit (Left k)) (Var "i")) (Bin Fall (Lit (Left (k-1))) (Bin Minus (Lit (Left k)) (Var "i")))))
            > suth DaI SUR True n k  = (Bin Times (Bin Choose (Lit (Left k)) (Lit (Left n))) (Bin Fall (Lit (Left (k-1))) (Lit (Left (k-n)))))
            > suth _   _   _    _ _  = (Lit (Left 0))



            > factorial :: Integer -> Integer -> Integer
            > factorial 0 k = k
            > factorial n k = factorial (n-1) (k*n)
            >
            > choose :: Integer -> Integer -> Integer
            > choose n k = (factorial n 1) `div` ((factorial (n-k) 1)*(factorial k 1))
            >
            > fall :: Integer -> Integer -> Integer
            > fall n k = (factorial n 1) `div` (factorial (n-k) 1)
            >
            > stir :: Integer -> Integer -> Integer
            > stir n k =    if n==k then 1 
            >   else if k==0 || k>n then 0 
            >   else (k*(stir (n-1) k)) + (stir (n-1) (k-1)) 
            >
            > fibgen :: Double -> Double -> Integer -> Double
            > fibgen x _ 0 = x
            > fibgen _ y 1 = y
            > fibgen x y z = ((fibgen x y (z-1)) + (fibgen x y (z-2)))
            >
            > part :: Integer -> Integer -> Integer
            > part _ 1 = 1
            > part n k | n < k = 0 
            > part n k = (part (n-1) (k-1)) + (part (n-k) (k))
            >
            > partition :: Integer -> Integer
            > partition 0 = 1
            > partition 1 = 1
            > partition 2 = 2
            > partition 3 = 3
            > partition 4 = 5
            > partition 5 = 7
            > partition 6 = 11
            > partition n = (partition (n-1)) + (partition (n-2)) - (partition (n-5)) 
            >             - (partition (n-7)) + (partitionHelper0 (n-12) 0) 

            PROOF:

            0    1     11   111   1111   11111     111111
                        2    21    211    2111      21111
                              3     22     221       2211
                                    31     311       3111
                                     4      41        411
                                            23        321
                                             5        222
                                                       51
                                                       42
                                                       33
                                                        6 



            > partitionHelper0 :: Integer -> Integer -> Integer
            > partitionHelper0 i c = if (i<0) then 0 else ((partition i) + (partitionHelper1 (i - 3 - c) (c+1)))
            >
            > partitionHelper1 :: Integer -> Integer -> Integer
            > partitionHelper1 i c = if (i<0) then 0 else ((partition i) - (partitionHelper2 (i - 5 - (2*c)) (c)))
            >
            > partitionHelper2 :: Integer -> Integer -> Integer
            > partitionHelper2 i c = if (i<0) then 0 else ((partition i) - (partitionHelper3 (i - 3 - c) (c+1)))
            >
            > partitionHelper3 :: Integer -> Integer -> Integer
            > partitionHelper3 i c = if (i<0) then 0 else ((partition i) + (partitionHelper0 (i - 5 - (2*c)) (c)))
            >
            > eTaylor :: Integer -> Double -> Double
            > eTaylor 0 e = e+1
            > eTaylor x e = e + (eTaylor (x-1) (e + (1 / (realToFrac (factorial (fromInteger x) (fromInteger x))))))


            



            e = sum 0->inf 1/i!
            pi = sum 0->inf 4/(2i+1)

            or 3.1415926535897926462


            How many mappings of n to k are there?
            (if 2 top -> (n < k) btm -> (k <= n))

                  reg/func              onto/surj     1-1/inj        bij 
            
            d.d          n^k                  n!*S(k,n)      n_k            n!

            d.i      sum 1-n S(k,i)          S(k,n)         0             0 (unless n=k then 1)
                  sum 0-n S(n,i)                         1             

            i.d     (n+k-1 choose k)   (k-1 choose n-1)   (n choose k)    0 (unless n=k then 1)

            i.i      sum 1-n P(k,i)           P(k,n)         1             0 (unless n=k then 1)
                  P(n)                            

            OM 
                              reg                                  onto

            d.d              (n+k-1)_k                       (k-1 chooose n-1)k!

            d.i   sum i-n (k choose i)((k-1)_(k-i))       (k choose n)((k-1)_(k-n))




            P(n) = total partitions of n

            P(n,k) = total partitions of n into k parts


            S(m,n) = S(m-1,n-1) + nS(m-1,n)

            1
            0  1
            0  1  1
            0  1  3  1
            0  1  7  6  1
            0  1  15 25 10 1
            ...

            Also S(n,k) = (1/k!) (sum_i=0^k (-1)^i {k choose i} (k-i)^n) 
