module Simplified_Expr_and_Stmt_data_types
   (Expr (..), Stmt (..), showWith, showLine, list) where

data Expr = Op String (Expr -> Expr -> Expr) Expr Expr
  | CommaExpr Expr
  | SemiColonExpr Expr
  | Negative Expr
  | Grouped Expr
  | Function String Expr
  | Id {char' :: Char}
  | Integer' {integer' :: Int}
  | Float' {float' :: Float}
  | String' {str' :: String}
  | Array Expr [Expr]
  | Bool' {bool' :: Bool}

instance Eq Expr where
  (Op s1 _ expr1 expr2) ==  (Op s2 _ expr3 expr4) = ((s1 == s2) && (expr1 == expr3) && (expr2 == expr4))
  (CommaExpr cs1) == (CommaExpr cs2) = (cs1 == cs2)
  (SemiColonExpr cs1) == (SemiColonExpr cs2) = (cs1 == cs2)
  (Negative expr1) == (Negative expr2) = (expr1 == expr2)
  (Grouped expr1) == (Grouped expr2) = (expr1 == expr2)
  (Function s1 _) == (Function s2 _) = (s1 == s2)
  (Id a) == (Id b) = (a == b) 
  (Integer' a) == (Integer' b) = (a == b)
  (Float' a) == (Float' b) = (a == b)
  (String' s1) == (String' s2) = (s1 == s2)
  (Array expr1 _) == (Array expr2 _) = (expr1 == expr2)
  (Bool' bool1) == (Bool' bool2) = (bool1 == bool2)
  _ == _ = False
  
instance Show Expr where
    show (Op cs _ e e') = show e ++ cs ++ show e'
--    show (NewLineExpr (String' cs)) = show cs
    show (CommaExpr cs) = show cs ++ "\t"
    show (SemiColonExpr cs) = show cs ++ " "
    show (Negative e) = "-" ++ show e
    show (Grouped e) = "(" ++ show e ++ ")"
    show (Function str e) = show str ++ " " ++ show e
    show (Id c) = [c]
    show (Integer' n) = show n
    show (Float' x) = show x
    show (String' cs) = cs    
    show (Array (Id c) es) = [c] ++ "(" ++ showWith ',' es ++ ")"
    show (Bool' bool)= show bool


data Stmt = REM String
  | LINE {linNum :: Int, stmt :: Stmt}
  | FOR {index :: Expr, start :: Expr , stop :: Expr, step :: Expr, body :: [Stmt]}
  | IF {test :: Expr, conseq :: Stmt}
  | PRINT [Expr]
  | GOTO {line :: Int}
  | GOSUB {line :: Int}    
  | ONGOTO Expr [Int]
  | INPUT {prompt :: String, vars :: [Expr]}
  | LET {lhs :: Expr, rhs :: Expr}
  | NEXT {indices' :: [Expr]}
  | DIM {arrays :: [Expr]}
  | RETURN
  | END
  deriving (Eq)

instance Show Stmt where
    show (LINE i s) = "LINE " ++ show i ++ ": " ++ show s
    show (REM cs) = "REM " ++ cs
    show (FOR i e e' (Integer' 1) body) = "FOR " ++ show i ++ "=" ++ show e ++ " TO " ++ show e' ++ " " ++ show body
    show (IF r (GOTO n)) = "IF " ++ show r ++ " THEN " ++ show n
    show (PRINT []) = ""
    show (PRINT es) = concatMap show es
    show (ONGOTO e ns) = "ON " ++ show e ++ " GOTO " ++ showWith ',' ns
    show (GOTO n) = "GOTO " ++ show n
    show (GOSUB n) = "GOSUB " ++ show n
    show (INPUT "" is) = "INPUT " ++ showWith ',' is
    show (INPUT cs is) = "INPUT " ++ show cs ++ ";" ++ showWith ',' is
    show (LET i e) = "LET " ++ show i ++ "=" ++ show e
    show (NEXT is) = "NEXT " ++ showWith ',' is    
    show (DIM es) = "DIM " ++ showWith ',' es
    show RETURN = "RETURN"
    show END = "END"

showWith _ [] = ""
showWith _ [x] = show x
showWith c (x:xs) = show x ++ [c] ++ showWith c xs

showLine (n, [stmt]) = putStrLn $ show n ++ " " ++ show stmt
showLine (n, stmts) = putStrLn $ show n ++ " " ++ showWith ':' stmts

list program = mapM_ showLine program
