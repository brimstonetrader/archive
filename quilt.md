      -- QUILT
      -- (%, sin, cos, tan, pi). I've also added the geometric transformations,
      -- rot(tz) = e^it * z, and dim [xmin,xmax,ymin,ymax] is a general scalar.
      -- magnitude and phase let you deal with polar coordinates. The user doesn't 
      -- need to know complex algebra, but might find 'm' or 'p' useful. These are my three
      -- extensions:

      -- import math        (trig, mod, raise, pi, e, abs, ceil, floor, round)
      -- import geom        (rot, dim [compresses a lot of geometric transformations into one command. 
      --                     input minx, maxx, miny, maxy, to make it bigger or move your viewing window around.
      --                     you can't go too far, something like -10 10 -10 10 is too much for it.].
      -- import lvl3        (magnitude, phase, ~ (draws a line from one coordinate to another. input "~ [x1] [x2] [y1] [y2]"), 
      --                     tiler (input "[t][qlit]", and your pixels will be emsmallened from 256*256 to t*t)
      --                     circle, square, and triangle [radius] [x1] [x2] do what you'd expect. polyg 5.0 [r] [cx] [cy] 
      --                     gives a pentagon, and so on.)

      {-# LANGUAGE GADTs #-}
      {-# OPTIONS_GHC -Wall #-}
      {-# LANGUAGE FlexibleInstances    #-}
      {-# LANGUAGE ViewPatterns         #-}

      module Quilt where

      import           Parsing2
      import           Prelude 
      import           Codec.Picture ()
      import           Data.Colour
      import           Data.Colour.Names
      import           Data.Colour.SRGB
      import           Data.Complex
      import           Data.Word
      import           Control.Exception (ArithException)

      -- | A color is a list of red, green, and blue values between 0.0 - 1.0.
      --   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
      --   darkish purple, and so on.

      type Color = [Double]

      -- | A quilt function produces a Color for any given location.  The
      --   parameter is a cartesian coordinate pixel location

      type QuiltFun =       Double -> Double -> Color


      evalQuilt :: String -> Either String QuiltFun
      evalQuilt s         = case parse quilt s of
        Left _           ->    Left $ "check your parentheses, there's been a parse error."
        Right e          ->
          case interpQuilt e of
            Left iErr    -> Left (show (showInterpError iErr))
            Right v      -> Right v


      lexer :: TokenParser u
      lexer = makeTokenParser $ emptyDef    
        { reservedNames = ["x","y","quilt","if","then","else","and","or","!","<",">","=","#",";","~","poly",
                          "+","-","*","/","%","^","sin","cos","tan","pi","e","round","floor","ceil","abs",
                          "c","m","p","rot","dim","refl","t","red","green","blue","white","black","true","false"] }

      data Quilt where
        Qool  :: QLit   -> Quilt  
        Color :: QLit   -> QLit  -> QLit  -> Quilt
        Dim   :: QLit   -> QLit  -> QLit  -> QLit  -> Quilt -> Quilt
        QBin  :: BOp    -> Quilt -> Quilt -> Quilt
        If    :: QLit   -> Quilt -> Quilt -> Quilt
        Quilt :: Quilt  -> Quilt -> Quilt -> Quilt -> Quilt 
        Rot   :: Double -> Quilt -> Quilt  
        deriving (Show) 

      data Coord where
        X     :: Coord
        Y     :: Coord  
        M     :: Coord
        P     :: Coord
        deriving (Show)

      data QLit where
        QBit  :: Double -> QLit
        Line  :: QLit   -> QLit -> QLit -> QLit -> QLit
        Shape :: DDDDD  -> QLit
        Uni   :: UOp    -> QLit -> QLit
        Bin   :: BOp    -> QLit -> QLit -> QLit
        CBin  :: COp    -> QLit -> QLit -> QLit
        Trig  :: Pytho  -> QLit -> QLit
        Tile  :: Double -> QLit -> QLit   
        W     :: Coord  -> QLit
        deriving (Show)

      data DDDDD where
        Ply :: Double -> Double -> Double -> Double -> DDDDD
        Tri :: Double  -> Double -> Double -> DDDDD
        Squ :: Double  -> Double -> Double -> DDDDD
        Ape :: Double  -> Double -> Double -> DDDDD
        deriving (Show)

      data UOp where
        Not   :: UOp
        Neg   :: UOp
        Abs   :: UOp
        Ceil  :: UOp
        Floor :: UOp
        Round :: UOp
        deriving (Show, Eq)

      data COp where
        Less  :: COp
        More  :: COp
        Same  :: COp
        Or    :: COp
        And   :: COp
        deriving(Show,Eq)

      data BOp where
        Plus  :: BOp
        Minus :: BOp
        Times :: BOp
        Raise :: BOp
        Div   :: BOp
        Mod   :: BOp
        deriving (Show, Eq)

      data Pytho where 
        Sin   :: Pytho
        Cos   :: Pytho
        Tan   :: Pytho
        deriving (Show, Eq)

      bhaskara :: Double -> Double
      bhaskara x =     (16*x*(3.14159265359 - x))/((5*(3.14159265359 ** 2))-(4*x*(3.14159265359 - x)))

      sine :: Double -> Double 
      sine 0 = 0
      sine x = if modulus x 6.28318530718 > 3.1415926535
            then truncate' ((-1) * bhaskara (modulus x 6.28318530718-3.1415926535)) 4
            else truncate' (bhaskara (modulus x 6.28318530718)) 4

      cosine :: Double -> Double
      cosine x = sine (x+(3.14159265359 / 2))

      tangent :: Double -> Double
      tangent x = sine x / cosine x

      -- x : number you want rounded, n : number of decimal places you want...
      -- https://stackoverflow.com/questions/18723381/rounding-to-specific-number-of-digits-in-haskell

      truncate' :: Double -> Int -> Double
      truncate' x n  =  (x * (10 ** realToFrac n)) / (10 ** realToFrac n)

      modulus :: Double -> Double -> Double
      modulus a b
        | (0 < a) && (a < b) = a
        |  0 > b     = (-1) * modulus a ((- 1) * b)
        |  a < b     = modulus (a + b) b
        |  otherwise = modulus (a - b) b


      ab :: Double -> Double
      ab a = case a of
        a | a>0 ->  a
        a | a<0 -> -a
        _       ->  0

      parens :: Parser a -> Parser a
      parens     = getParens lexer

      reservedOp :: String -> Parser ()
      reservedOp = getReservedOp lexer

      reserved :: String -> Parser ()
      reserved   = getReserved lexer

      double :: Parser Double
      double  = getFloat lexer



      parseQLitAtom :: Parser QLit
      parseQLitAtom = try (parens parseQLit)
        <|>           try (QBit                  <$>  double)
        <|>           try (Uni  Not              <$> (reserved "!"   *> parseQLit))
        <|>           try (Uni  Neg              <$> (reservedOp "-"   *> parseQLit))
        <|>           try (Uni  Abs              <$> (reserved "abs"   *> parseQLit))
        <|>           try (Uni  Round            <$> (reserved "o"   *> (parens parseQLit)))  
        <|>           try (Uni  Ceil             <$> (reserved ";"   *> (parens parseQLit)))
        <|>           try (Uni  Floor            <$> (reserved "#"   *> (parens parseQLit)))
        <|>           try (Trig Sin              <$> (reserved "sin " *> parseQLit))
        <|>           try (Trig Cos              <$> (reserved "cos " *> parseQLit))
        <|>           try (Trig Tan              <$> (reserved "tan " *> parseQLit))
        <|>           try (QBit 3.14159265359    <$   reserved "pi"   )
        <|>           try (QBit 2.718281828459   <$   reserved "e"    )
        <|>           try (W X                   <$   char 'x'  )
        <|>           try (W Y                   <$   char 'y'  )
        <|>           try (W M                   <$   char 'm'  )
        <|>           try parseCircle
        <|>           try parseTriangle
        <|>           try parseSquare
        <|>           try parsePolygon
        <|>           try (W P                   <$   char 'p'  )
        <|>           try (QBit 0.0              <$   reserved "false")
        <|>           try (QBit 1.0              <$   reserved "true" )
        <|>           try parseLine
        <|>           try parseTile
        <|>           try (QBit 1.0 <$ whiteSpace)


      parseTile :: Parser QLit
      parseTile = do
        _ <- char '['
        _ <- whiteSpace 
        n <- double
        _ <- whiteSpace
        _ <- char ']'
        _ <- whiteSpace
        q <- parseQLit
        return $ Tile n q


      parseCircle :: Parser QLit
      parseCircle = do
        _ <- reserved "circle"
        _ <- whiteSpace 
        r <- double
        a <- double
        b <- double
        return $ Shape (Ape r a b)

      parseSquare :: Parser QLit
      parseSquare = do
        _ <- reserved "square"
        _ <- whiteSpace 
        r <- double
        a <- double
        b <- double
        return $ Shape (Squ r a b)

      parsePolygon :: Parser QLit
      parsePolygon = do
        _ <- reserved "polyg"
        _ <- whiteSpace 
        n <- double
        _ <- whiteSpace
        r <- double
        a <- double
        b <- double
        return $ Shape (Ply n r a b)

      parseTriangle :: Parser QLit
      parseTriangle = do
        _ <- reserved "triangle"
        _ <- whiteSpace 
        r <- double
        a <- double
        b <- double
        return $ Shape (Tri r a b)



      parseLine :: Parser QLit
      parseLine = do
        _ <- oneOf "~"
        _ <- whiteSpace 
        x1 <- parseQLit  
        _ <- whiteSpace 
        x2 <- parseQLit  
        _ <- whiteSpace 
        y1 <- parseQLit  
        _ <- whiteSpace   
        y2 <- parseQLit  
        _ <- whiteSpace 
        return $ Line x1 x2 y1 y2


      parseQuiltAtom :: Parser Quilt
      parseQuiltAtom = try (parens parseQuilt)
                  <|> try parseColor
                  <|> try parseIf 
                  <|> try parseQubit 
                  <|> try parseRot 
                  <|> try parseDim 
                  <|> try (Qool <$> parseQLit)
                  <|> try parseReserved



      parseReserved :: Parser Quilt
      parseReserved = (Color (QBit 1.0) (QBit 0.0) (QBit 0.0)) <$  reserved "red"
                <|>   (Color (QBit 0.0) (QBit 1.0) (QBit 0.0)) <$  reserved "green"
                <|>   (Color (QBit 0.0) (QBit 0.0) (QBit 1.0)) <$  reserved "blue" 
                <|>   (Color (QBit 1.0) (QBit 1.0) (QBit 1.0)) <$  reserved "white"
                <|>   (Color (QBit 0.0) (QBit 0.0) (QBit 0.0)) <$  reserved "black"            
                <|>   (Color (QBit 1.0) (QBit 1.0) (QBit 1.0)) <$  reserved "true"
                <|>   (Color (QBit 0.0) (QBit 0.0) (QBit 0.0)) <$  reserved "false"


      parseRot :: Parser Quilt
      parseRot = do
        _ <- reserved "rot"
        etotheitauthis <- double
        q             <- parseQuilt
        return $ Rot etotheitauthis q

      parseDim :: Parser Quilt
      parseDim = do
        _ <- reserved "dim"
        _ <- whiteSpace
        w <- parseQLit
        _ <- whiteSpace
        x <- parseQLit
        _ <- whiteSpace
        y <- parseQLit
        _ <- whiteSpace
        z <- parseQLit
        _ <- whiteSpace
        q <- parseQuilt
        return $ Dim w x y z q


      parseQubit :: Parser Quilt
      parseQubit = do
        _ <- reserved "quilt"
        nw <- parseQuilt
        ne <- parseQuilt
        sw <- parseQuilt
        se <- parseQuilt
        return $ Quilt nw ne sw se
        
      parseIf :: Parser Quilt
      parseIf = do
        _ <- reserved "if"
        cond <- parseQLit
        _ <- reserved "then"
        th   <- parseQuilt
        _ <- reserved "else"
        el   <- parseQuilt
        return $ If cond th el
        
        
        

      parseColor :: Parser Quilt 
      parseColor = do
        _ <- reservedOp "["
        r <- parseQLit
        _ <- whiteSpace
        _ <- reservedOp ","
        _ <- whiteSpace
        g <- parseQLit
        _ <- whiteSpace
        _ <- reservedOp ","
        _ <- whiteSpace
        b <- parseQLit
        _ <- reservedOp "]"
        return $ Color r g b





      parseQLit :: Parser QLit
      parseQLit = buildExpressionParser table parseQLitAtom
        where
          table =    [[ Infix (Bin  Raise <$ reservedOp "^"  ) AssocRight
                    ],[ Infix (Bin  Times <$ reservedOp "*"  ) AssocLeft
                      , Infix (Bin  Div   <$ reservedOp "/"  ) AssocLeft
                      , Infix (Bin  Mod   <$ reservedOp "%"  ) AssocLeft
                    ],[ Infix (CBin Less  <$ reservedOp "<"  ) AssocLeft
                      , Infix (CBin More  <$ reservedOp ">"  ) AssocLeft
                      , Infix (CBin Same  <$ reservedOp "="  ) AssocLeft
                    ],[ Infix (CBin And   <$ reserved   "and") AssocLeft
                      , Infix (CBin Or    <$ reserved   "or" ) AssocLeft
                    ],[ Infix (Bin  Plus  <$ reservedOp "+"  ) AssocLeft
                      , Infix (Bin  Minus <$ reservedOp "-"  ) AssocLeft
                      ]
                    ]

      parseQuilt :: Parser Quilt
      parseQuilt = buildExpressionParser table parseQuiltAtom
        where
          table =    [[ Infix (QBin Raise <$ reservedOp "^"  ) AssocRight],
                      [ Infix (QBin Times <$ reservedOp "*"  ) AssocLeft  ,
                        Infix (QBin Div   <$ reservedOp "/"  ) AssocLeft  ,
                        Infix (QBin Mod   <$ reservedOp "%"  ) AssocLeft ],
                      [ Infix (QBin Plus  <$ reservedOp "+"  ) AssocLeft
                    ,  Infix (QBin Minus <$ reservedOp "-"  ) AssocLeft
                      ]
                    ]                          

      natural :: Parser Integer
      natural    = getInteger lexer

      naturalOrFloat :: Parser (Either Integer Double)
      naturalOrFloat = getNaturalOrFloat lexer 

      whiteSpace :: Parser ()
      whiteSpace = getWhiteSpace lexer


      identifier :: Parser String
      identifier = getIdentifier lexer

      redRGB :: Parser QLit
      redRGB   = ((oneOf "[") *> parseQLit)

      greenRGB :: Parser QLit
      greenRGB = ((oneOf ",") *> parseQLit)

      blueRGB :: Parser QLit
      blueRGB  = ((oneOf ",") *> parseQLit) <* (oneOf "]")

      quilt :: Parser Quilt
      quilt = whiteSpace *> parseQuilt <* eof

      qlit :: Parser QLit
      qlit = whiteSpace *> parseQLit <* eof


      data InterpError where
        DivByZero  :: InterpError
        YouFucked  :: InterpError

      showParseError :: ParseError -> String
      showParseError YouFucked2      = "here king u dropped these: ))((()"

      data ParseError where
        YouFucked2  :: ParseError

      showInterpError :: InterpError -> String
      showInterpError DivByZero      = "Division by zero"
      showInterpError YouFucked      = "Don't do that!!"

      interpQuilt :: Quilt -> Either InterpError QuiltFun
      interpQuilt (Color r g b)       = Right $ \x y ->   [interpQLit x y r, interpQLit x y g, interpQLit x y b]
      interpQuilt (Qool  c)           = Right $ \x y ->   [interpQLit x y c, interpQLit x y c,interpQLit x y c]
      interpQuilt (Quilt q1 q2 q3 q4) = Right $ \x y ->    qubit q1 q2 q3 q4 x y
      interpQuilt (If    bl q1 q2)    = Right $ \x y -> case (if interpQLit  x y bl >= 0.5 then interpQuilt q1 else interpQuilt q2) of
        Left _  -> [0,0,0]
        Right z -> z x y                                            
      interpQuilt (Rot   th q)        = Right $ \x y -> zob q (interpGeom   x y (Left th))
      interpQuilt (Dim   j k l m q)   = Right $ \x y -> zob q (interpGeom   x y (Right [(interpQLit x y j), (interpQLit x y k), (interpQLit x y l), (interpQLit x y m)]))
      interpQuilt (QBin  op q1 q2)    = Right $ \x y -> interpQuiltBin x y op q1 q2

      -- 0.4 0.5 Raise (Color (PZ) (Y) (QBit 0.8)) (Qool MZ)  
      -- [0.9321362867960192,0.6415739972710967,0.8668567088949923]

      interpQuiltBin :: Double -> Double -> BOp -> Quilt -> Quilt -> Color
      interpQuiltBin x y op q1 q2 =   case (interpQuilt q1, interpQuilt q2) of
        (Right qf1, Right qf2)     -> case (qf1 x y,               qf2 x y) of
          ([r0,g0,b0], [r1,g1,b1]) -> [    (interpQLit x y (Bin op (QBit r0) (QBit r1))), 
                                          (interpQLit x y (Bin op (QBit g0) (QBit g1))), 
                                          (interpQLit x y (Bin op (QBit b0) (QBit b1)))]
          _ -> [0,0,0]
        _ ->   [0,0,0]

        
      interpGeom :: Double -> Double -> Either Double [Double] -> Complex Double
      interpGeom x y s = case s of
        Right [j,k,l,m] -> if (m==1.0532889323423415489) 
                      then (((x/(1+(j*j)))*((1-(j*j)) + (2*j))) :+ 
                            ((y/(1+(j*j)))*((1-(j*j)) + (2*j))))                                   
                      else ((j*(x-1))+(k*(x+1)))/2 :+ ((l*(y-1))+(m*(y+1)))/2
        Left th         -> (x :+ y) * 2.718281828459 ** ((0 :+ 1) * 3.14159265359 * 2 * (th :+ 0))
        _               ->  0 :+ 0

      --reflection did not wind up working out.


      zob :: Quilt -> Complex Double -> Color
      zob (Color r g b)       (c :+ d) =      [interpQLit c d r, interpQLit c d g, interpQLit c d b] 
      zob (Qool p)            (c :+ d) =      [interpQLit c d p, interpQLit c d p, interpQLit c d p]
      zob (Quilt q1 q2 q3 q4) (c :+ d) =  if   (c==0) || (d==0) 
                                          then [0,0,0] else 
                                          case (c >0,     d >0) of   
                                            (True , True)  -> case interpQuilt q2 of
                                              Left _ -> [0,0,0]
                                              Right z -> z c d
                                            (True , False) -> case interpQuilt q4 of
                                              Left _ -> [0,0,0]
                                              Right z -> z c d
                                            (False, True)  -> case interpQuilt q1 of
                                              Left _ -> [0,0,0]
                                              Right z -> z c d
                                            (False, False) -> case interpQuilt q3 of
                                              Left _ -> [0,0,0]
                                              Right z -> z c d
      zob (If   p q1 q2)      (c :+ d) =  case (if interpQLit  c d p >= 0.5 then interpQuilt q1 else interpQuilt q2) of
        Left _  -> [0,0,0]
        Right z -> z c d               
      zob (Rot  th q)         (c :+ d) =  zob q (interpGeom c d (Left th))
      zob (Dim  j k l m q)    (c :+ d) =  zob q (interpGeom c d (Right [(interpQLit c d j), (interpQLit c d k), (interpQLit c d l), (interpQLit c d m)]))
      zob (QBin op q1 q2)     (c :+ d) =  interpQuiltBin c d op q1 q2

      interpQLit :: Double -> Double -> QLit -> Double
      interpQLit x y (QBit n)              = case n of
        n | n<0.0        -> interpQLit x y (QBit ((-1.0)*n))
        n | n>1.0        -> interpQLit x y (QBit (modulus n 1.0))
        _                -> n
      interpQLit x _ (W X)            = x
      interpQLit _ y (W Y)            = y
      interpQLit x y (W M)            = ((x*x) + (y*y)) ** 0.5 --or e ** (re log z)
      interpQLit x y (W P)            = (((phase   (x :+ y))  / ((3.14159265359)*2))+0.5)    --or im log z
      interpQLit x y (Uni Not z)      =                      1 - interpQLit x y z
      interpQLit x y (Uni Neg z)      =                      0 - interpQLit x y z
      interpQLit x y (Uni Abs z)      =                      ab (interpQLit x y z)
      interpQLit x y (Uni Ceil z)     =              realToFrac(  ceiling (interpQLit x y z))
      interpQLit x y (Uni Floor z)    =             realToFrac(   floor (interpQLit x y z))
      interpQLit x y (Uni Round z)    =             realToFrac(   round (interpQLit x y z))
      interpQLit x y (Trig Sin z)     =                    sine (interpQLit x y z)
      interpQLit x y (Trig Cos z)     =                  cosine (interpQLit x y z)
      interpQLit x y (Trig Tan z)     =                 tangent (interpQLit x y z)
      interpQLit x y (Tile i z)       =    interpQLit  
        (realToFrac (round (realToFrac i * x)) / realToFrac i) 
        (realToFrac (round (realToFrac i * y)) / realToFrac i) z
      interpQLit x y (Bin Plus z w)   =    interpQLit x y z +    interpQLit x y w
      interpQLit x y (Bin Minus z w)  =    interpQLit x y z -    interpQLit x y w
      interpQLit x y (Bin Times z w)  =    interpQLit x y z *    interpQLit x y w
      interpQLit x y (Bin Raise z w)  =    interpQLit x y z **   interpQLit x y w
      interpQLit x y (Bin Div z w)    = case                     interpQLit x y w of
          0.0 -> 1.0                                                                                                      --i'd argue something abt floating point modular division doing a closure thing like a stereographic proje
          w0 ->                                  (interpQLit x y z) / w0
      interpQLit x y (Bin Mod z w)    = modulus   (interpQLit x y z)  (interpQLit x y w)
      interpQLit x y (CBin Less z w)  =  if       (interpQLit x y z >  interpQLit x y w) then (1.0) else (0.0)
      interpQLit x y (CBin More z w)  =  if       (interpQLit x y z <  interpQLit x y w) then (1.0) else (0.0)
      interpQLit x y (CBin Same z w)  =  if       (interpQLit x y z == interpQLit x y w) then (1.0) else (0.0)
      interpQLit x y (CBin Or z w)    =            interpQLit x y (Bin Plus z w) 
      interpQLit x y (CBin And z w)   =            interpQLit x y (Bin Times z w)
      interpQLit x y (Line a b c d)   = do 
        let x1 = interpQLit x y a
        let x2 = interpQLit x y b
        let y1 = interpQLit x y c
        let y2 = interpQLit x y d
        if (((y - y1)>(0.9*(((y2-y1)/(x2-x1))*(x-x1))))&&
            ((y - y1)<(1.1*(((y2-y1)/(x2-x1))*(x-x1))))&&
            (y1<y)&&(y2>y)&&(x1<x)&&(x2>x))    then 1.0 else 0.0
      interpQLit x y (Shape (Ape   r a b))   = if ((((x-a)*(x-a)) + ((y-b)*(y-b)) - r)<0.0)               then 1.0 else 0.0
      interpQLit x y (Shape (Tri r a b) )    = if (((abs (x-a)) + (abs ((abs (x-a)) + (y-b))) - r) < 0.0) then 1.0 else 0.0
      interpQLit x y (Shape (Squ   r a b))   = if (((abs (x-a)) + (abs (y-b)) - r) < 0.0)                 then 1.0 else 0.0
      interpQLit x y (Shape (Ply   n r a b)) = if (((cosine (3.14159265359 / n))/(cosine ((phase   (x :+ y)) - (((2*3.14159265359)/n)*
        (realToFrac (floor (((n*(phase   (x :+ y)))+3.14159265359)/(3.14159265359*2)))))))) - (((x-a)*(x-a)) + ((y-b)*(y-b)))) < 0.0 then 1.0 else 0.0
      interpQLit x y (Tile t z)              = interpQLit (realToFrac ((realToFrac (truncate (t*x)))/t)) 
                                                          (realToFrac ((realToFrac (truncate (t*y)))/t)) z




      -- sthRootsOfUnity :: Integer -> Integer -> Double -> Double -> [Complex Double]
      -- sthRootsOfUnity sd sn x y = if ((sn)==sd) then 
      --    ([((2.718281828459 :+ 0.0) ** (0.0 :+ (2.0*3.14159265359*((realToFrac sn)/(realToFrac sd)))))]) else(
      --   ((2.718281828459 :+ 0.0) ** (0.0 :+ (2.0*3.14159265359*((realToFrac sn)/(realToFrac sd))))):(sthRootsOfUnity sd (sn+1) x y)) 

      -- ruRenderer :: Double -> Double -> [Complex Double] -> Double
      -- ruRenderer x y []      = 0.0
      -- ruRenderer x y ((a :+ b):xs)  = if ((x>(0.9*a))&&(x<(1.1*a))&&(y>(0.9*b))&&(y<(1.1*b))) then 1.0 else
      --   (ruRenderer x y xs)

      qubit :: Quilt -> Quilt -> Quilt -> Quilt -> Double -> Double -> Color
      qubit q1 q2 q3 q4 c d = (if (c==0) || (d==0) 
        then [0,0,0] else case (c >0,     d >0) of   
          (True , True)  -> case interpQuilt q2 of
            Left _ -> [0,0,0]
            Right z -> z c d
          (True , False) -> case interpQuilt q4 of
            Left _ -> [0,0,0]
            Right z -> z c d
          (False, True)  -> case interpQuilt q1 of
            Left _ -> [0,0,0]
            Right z -> z c d
          (False, False) -> case interpQuilt q3 of
            Left _ -> [0,0,0]
            Right z -> z c d)


