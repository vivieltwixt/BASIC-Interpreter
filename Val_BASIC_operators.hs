module Val_BASIC_operators (module Val_BASIC_operators) where
import Simplified_Expr_and_Stmt_data_types

-- Binary Arithmetic Operators
(+) :: Expr -> Expr -> Expr
(+) (Integer' expr1) (Integer' expr2) = Integer' (expr1 Prelude.+ expr2)
(+) (Float' expr1) (Float' expr2) = Float' (expr1 Prelude.+ expr2)

(-) :: Expr -> Expr -> Expr
(-) (Integer' expr1) (Integer' expr2) = Integer' (expr1 Prelude.- expr2)
(-) (Float' expr1) (Float' expr2) = Float' (expr1 Prelude.- expr2)

(*) :: Expr -> Expr -> Expr
(*) (Integer' expr1) (Integer' expr2) = Integer' (expr1 Prelude.* expr2)
(*) (Float' expr1) (Float' expr2) = Float' (expr1 Prelude.* expr2)

(/) :: Expr -> Expr -> Expr
(/) (Integer' expr1) (Integer' expr2) = Float' ((fromIntegral expr1) Prelude./ (fromIntegral expr2))
(/) (Float' expr1) (Float' expr2) = Float' (expr1 Prelude./ expr2)


(^) :: Expr -> Expr -> Expr
(^) (Integer' expr1) (Integer' expr2) = Float' ((fromIntegral expr1) Prelude.** (fromIntegral expr2))
(^) (Float' expr1) (Float' expr2) = Float' (expr1 Prelude.** expr2)


-- Binary Boolean Operators
or' :: Expr -> Expr -> Expr
or' (Bool' expr1) (Bool' expr2) = Bool' (or [expr1, expr2])

and' :: Expr -> Expr -> Expr
and' (Bool' expr1) (Bool' expr2) = Bool' (and [expr1, expr2])
  
(<>) :: Expr -> Expr -> Expr
(<>) (Integer' expr1) (Integer' expr2) = Bool' (expr1 /= expr2)
(<>) (Float' expr1) (Float' expr2) = Bool' (expr1 /= expr2)

(<=) :: Expr -> Expr -> Expr
(<=) (Integer' expr1) (Integer' expr2) = Bool' (expr1 Prelude.<= expr2)
(<=) (Float' expr1) (Float' expr2) = Bool' (expr1 Prelude.<= expr2)

(>=) :: Expr -> Expr -> Expr
(>=) (Integer' expr1) (Integer' expr2) = Bool' (expr1 Prelude.>= expr2)
(>=) (Float' expr1) (Float' expr2) = Bool' (expr1 Prelude.>= expr2)

(==) :: Expr -> Expr -> Expr
(==) (Integer' expr1) (Integer' expr2) = Bool' (expr1 Prelude.== expr2)
(==) (Float' expr1) (Float' expr2) = Bool' (expr1 Prelude.== expr2)

(>) :: Expr -> Expr -> Expr
(>) (Integer' expr1) (Integer' expr2) = Bool' (expr1 Prelude.> expr2)
(>) (Float' expr1) (Float' expr2) = Bool' (expr1 Prelude.> expr2)

(<) :: Expr -> Expr -> Expr
(<) (Integer' expr1) (Integer' expr2) = Bool' (expr1 Prelude.< expr2)
(<) (Float' expr1) (Float' expr2) = Bool' (expr1 Prelude.< expr2)
