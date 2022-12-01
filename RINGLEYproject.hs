
-- Hunter Ringley
-- Final Project
-- Numerical Integration
-- CS 3490 Fall 2022

import Data.Char

type Vars = String

data AExpr = Var Vars | Const Double | Add AExpr AExpr | Mul AExpr AExpr
           | Sub AExpr AExpr | Div AExpr AExpr | Sin AExpr | Cos AExpr
           | Tan AExpr | Ln AExpr | Exp AExpr AExpr
    deriving (Show,Eq)

data BinOps = PlusOp | MulOp | DivOp | SubOp | ExpOp
    deriving (Show,Eq)

data UOps = SinOp | CosOp | TanOp | LnOp
    deriving (Show,Eq)

data Token = BinOp BinOps | UOp UOps | CSym Double | VSym String | LPar | RPar | PE AExpr | Err String
    deriving (Show,Eq)

-- constants for recursive constraints
tolerance = 1e-15
maxDepth = 13


parseExpr :: [Token] -> AExpr
parseExpr = sr []

sr :: [Token] -> [Token] -> AExpr
sr [PE e] [] = e
sr (PE e   : PE (Const c)         : s) q = sr (PE (Mul (Const c) e) : s) q
sr (VSym x                        : s) q = sr (PE (Var x)           : s) q
sr (CSym c                        : s) q = sr (PE (Const c)         : s) q
sr (PE e2  : BinOp PlusOp : PE e1 : s) q = sr (PE (Add e1 e2)       : s) q
sr (PE e2  : BinOp MulOp  : PE e1 : s) q = sr (PE (Mul e1 e2)       : s) q
sr (PE e2  : BinOp SubOp  : PE e1 : s) q = sr (PE (Sub e1 e2)       : s) q
sr (PE e2  : BinOp DivOp  : PE e1 : s) q = sr (PE (Div e1 e2)       : s) q
sr (PE e2  : BinOp ExpOp  : PE e1 : s) q = sr (PE (Exp e1 e2)       : s) q
sr (PE e   : UOp   SinOp          : s) q = sr (PE (Sin e)           : s) q
sr (PE e   : UOp   CosOp          : s) q = sr (PE (Cos e)           : s) q
sr (PE e   : UOp   TanOp          : s) q = sr (PE (Tan e)           : s) q
sr (PE e   : UOp   LnOp           : s) q = sr (PE (Ln e)            : s) q
sr (RPar   : PE e         :  LPar : s) q = sr (PE e                 : s) q
sr s (x:xs) = sr (x:s) xs
sr s [] = error ("Parse error: " ++ show s)

lexer :: String -> [Token]
lexer "" = []
lexer ('(':s)         = LPar         : lexer s
lexer (')':s)         = RPar         : lexer s
lexer ('*':s)         = BinOp MulOp  : lexer s
lexer ('+':s)         = BinOp PlusOp : lexer s
lexer ('/':s)         = BinOp DivOp  : lexer s
lexer ('-':s)         = BinOp SubOp  : lexer s
lexer ('^':s)         = BinOp ExpOp  : lexer s
lexer ('s':'i':'n':s) = UOp   SinOp  : lexer s
lexer ('c':'o':'s':s) = UOp   CosOp  : lexer s
lexer ('t':'a':'n':s) = UOp   TanOp  : lexer s
lexer ('l':'n':s)     = UOp   LnOp   : lexer s
lexer ('x':s)         = VSym "x"     : lexer s
lexer (c:s) | isDigit c =
  let (num,rst) = span (\x -> isDigit x || x == '.') s
   in CSym (read (c:num)) : lexer rst
-- lexer (c:s) | isAlpha c =
--   let (num,rst) = span (\x -> isAlphaNum x || x == '_') s
--    in VSym (c:num) : lexer rst
lexer (c:s) | isSpace c = lexer s
lexer s = [Err (show s)]

parser :: String -> AExpr
parser = parseExpr . lexer

eval :: (Vars,Double) -> AExpr -> Double
eval (n,d) (Var v)     = d
eval env   (Const n)   = n
eval env   (Add e1 e2) = eval env e1 + eval env e2
eval env   (Mul e1 e2) = eval env e1 * eval env e2
eval env   (Sub e1 e2) = eval env e1 - eval env e2
eval env   (Div e1 e2) = eval env e1 / eval env e2
eval env   (Exp e1 e2) = eval env e1 ** eval env e2
eval env   (Sin e)     = sin (eval env e)
eval env   (Cos e)     = cos (eval env e)
eval env   (Tan e)     = tan (eval env e)
eval env   (Ln e)      = log (eval env e)


-- twoPoint :: Double -> Double -> AExpr -> Double
-- twoPoint a b expr = n * (eval [("x", (n * (-1.0 / sqrt 3.0) + m))] expr) + n * (eval [("x", (n * (1.0 / sqrt 3.0) + m))] expr)
--             where
--                 m = (a + b) / 2.0
--                 n = (b - a) / 2.0


threePoint :: Double -> Double -> AExpr -> Double
threePoint a b expr = n * ((5.0 / 9.0) * (eval ("x", (n * (- sqrt(3.0 / 5.0)) + m)) expr) + (8.0 / 9.0) * (eval ("x", m) expr) + (5.0 / 9.0) * (eval ("x", (n * (sqrt(3.0 / 5.0)) + m)) expr))
            where
                m = (a + b) / 2.0
                n = (b - a) / 2.0

adaptiveThree :: Double -> Double -> Integer -> Integer -> Double -> AExpr -> Double
adaptiveThree a b depth maxDepth tol expr | depth >= maxDepth = threePoint a b expr
adaptiveThree a b depth maxDepth tol expr | abs (whole - left + right) > tol = 
        (adaptiveThree a midpoint (depth + 1) maxDepth (tol / 2) expr) + (adaptiveThree midpoint b (depth + 1) maxDepth (tol / 2) expr)
            where
                midpoint = (a + b) / 2
                whole = threePoint a b expr
                left = threePoint a midpoint expr
                right = threePoint midpoint b expr
adaptiveThree a b depth maxDepth tol expr = threePoint a b expr


main :: IO ()
main = putStrLn "Enter a choice of eval, int, or quit." >>
  getLine >>= (\s -> case s of
  "eval" -> do
    putStrLn "Enter a string:"
    s <- getLine
    let ps = parseExpr $ lexer s
    putStrLn "The result of parser is:"
    putStrLn (show (ps))
    putStrLn "The result of evaluation is:"
    putStrLn (show (eval ("x",0) ps))
    main
  "int" -> do
    putStrLn "Enter a function:"
    putStr "f(x) = "
    s <- getLine
    putStrLn "Enter lower bound:"
    lower <- getLine
    putStrLn "Enter upper bound:"
    upper <- getLine
    let a = read lower :: Double
    let b = read upper :: Double
    let ls = lexer s
    putStrLn "The result of parser is:"
    putStrLn (show (parseExpr ls))
    putStrLn "The result of integration is:"
    putStrLn (show (adaptiveThree a b 0 maxDepth tolerance (parseExpr ls)))
    main
  "quit" -> return ()
  _ -> do
    putStrLn $ "Invalid input: " ++ s
    main 
  )