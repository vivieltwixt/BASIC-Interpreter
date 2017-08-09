module Val_BASIC_parser (module Val_BASIC_parser, module App, module Simplified_Expr_and_Stmt_data_types, module Operators, module Data.Char) where
import qualified Control.Applicative as App
import Simplified_Expr_and_Stmt_data_types
import Parselib
import qualified Val_BASIC_operators as Operators
import Data.Char

--- Basic Parsers ---
printable = (/= '"')

ws = many $ sat (App.liftA2 (&&) (App.liftA2 (&&) (isSpace) (/= '\r')) (/= '\n'))
newLine = (do {char '\r'; char '\n'}) +++ char '\n'  
string' = do {char '"'; ws; prints <- many1 (sat printable); ws; char '"'; return (String' prints)}
letter' = do {i <- letter; return (Id i)}

line' :: Parser Stmt
line' = do {ws; i <- integer; ws; s <- statement; ws; char '\n'; return (LINE i s)}

lines' :: Parser [Stmt]
lines' = many1 line'


statements :: Parser [Stmt]
statements = many1 statement

statement :: Parser Stmt
statement = state1 +++ state2 +++ state3 +++ state4 +++ state5 +++ state6 +++ state7 +++ state9 +++ state10 +++ state11 +++ state12 +++ state13 +++ state14 +++ state15 +++ state16 +++ state17 +++ state18 +++ state19 +++ state20 +++ state21
state1 = do {string "DIM"; ws; a <- arrayList; return (DIM a)} 
state2 = do {string "END"; return END}
state3 = do {string "FOR"; ws; index <- letter'; ws; char '='; ws; start <- expression; ws;
             string "TO"; ws; stop <- expression; ws; char '\n'; ws; body <- lines'; ws; end <- next;
             return (FOR index start stop (Integer' 1) (body++[end]))}
state4 = do {string "FOR"; ws; index <- letter'; ws; char '='; ws; start <- expression; ws;
             string "TO"; ws; stop <- expression; string "STEP"; step <- expression; ws; char '\n'; body <- lines'; ws; end <- next;
             return (FOR index start stop step (body ++ [NEXT [index]]))}
state5 = do {string "GOTO"; ws; i <- integer; return (GOTO i)}
state6 = do {string "GOSUB"; ws; i <- integer; return (GOSUB i)}
state7 = do {string "IF"; ws; test <- expression; ws; string "THEN"; ws; i <- integer; ws; return (IF test (GOTO i))}
state8 = do {string "ON"; ws; expression; ws; string "GOTO"; ws; intList}
state9 = do {string "INPUT"; ws; prompt <- string'; ws; char ';'; ws; vars <- letters; return (INPUT (str' prompt) vars)}
state10 = do {string "INPUT"; ws; vars <- letters; return (INPUT "" vars)}
state12 = do {string "LET"; ws; lhs <- array; ws; char '='; ws; rhs <- expression; return (LET lhs rhs)}
state11 = do {string "LET"; ws; lhs <- letter'; ws; char '='; ws; rhs <- expression; return (LET lhs rhs)}
state13 = do {string "PRINT TAB"; ws; char '('; ws; i <- integer; ws; char ')'; ws; char ';'; ws; exprs <- printList; return (PRINT ([String' (replicate i '\t')] ++ exprs))}
state14 = do {string "PRINT"; ws; arrs <- arrayList; return (PRINT arrs)}
state15 = do {string "PRINT"; ws; exprs <- printList; return (PRINT exprs)}
state16 = do {string "PRINT"; ws; exprs <- expression'; return (PRINT exprs)}
state17  = do {string "PRINT"; ws; return (PRINT [String' ""])}
state18 = do {string "REM"; ws; s <- many1 (sat $ (/= '\n')); return (REM s)}
state19 = do {string "RETURN"; return (RETURN)}
state20 = do {lhs <- variable; ws; char '='; ws; rhs <- expression; return (LET lhs rhs)}
state21 = do {string "ON"; ws; l <- letter'; ws; string "GOTO"; ws; is <- intList; return (ONGOTO l is)}

next  = do {ws; i <- integer; ws; string "NEXT"; ws ; indices' <- many1 letter'; ws; return (LINE i (NEXT indices'))}

expression :: Parser Expr
expression = function +++ expr +++ andExpr
expr = do {ae <- andExpr; ws; string "OR"; ws; e <- expression; return (Op "OR" Operators.or' ae e)}


letters = ls +++ letter''
ls = do {l <- letter'; ws; char ','; ws; ls <- letters; return ([l]++ls)}
letter'' = do {l <- letter'; return [l]}


arrayList = arrs +++ array'
arrs = do {a <- array; ws; char ','; ws; as <- arrayList; return ([a]++as)}
array' = do {a <- array; return [a]}


intList = ints +++ ints2
ints = do {i <- integer; ws; char ','; ws; is <- intList; return ([i]++is)}
ints2 = do {i <- integer; return [i]}


expressionList = exprs +++ expression'
exprs = do {e <- expression; ws; char ','; ws; elist <- expressionList; return ([e]++elist)}
expression' = do {e <- expression; return [e]}


printList = prints1 +++ prints2 +++ expression'
prints1 = do {e <- expression; ws; char ','; ws; plist <- printList; ws; many (char ','); return ([CommaExpr e]++plist)}
prints2 = do {e <- expression; ws; char ';'; ws; plist <- printList; ws; many (char ';'); return ([SemiColonExpr e]++plist)}

andExpr = andEx +++ compareExpr
andEx = do {ne <- compareExpr; ws; string "AND"; ws; ae <- andExpr; return (Op "AND" Operators.and' ne ae)}

compareExpr = compExp1 +++ compExp2 +++ compExp3 +++ compExp4 +++ compExp5 +++ compExp6 +++ addExpr
compExp1 = do {ae <- addExpr; ws; char '='; ws; ce <- compareExpr; return (Op "=" (Operators.==) ae ce)}
compExp2 = do {ae <- addExpr; ws; char '>'; ws; ce <- compareExpr; return (Op ">" (Operators.>) ae ce)}
compExp3 = do {ae <- addExpr; ws; char '<'; ws; ce <- compareExpr; return (Op "<" (Operators.<) ae ce)}
compExp4 = do {ae <- addExpr; ws; string "<>"; ws; ce <- compareExpr; return (Op "<>" (Operators.<>) ae ce)}
compExp5 = do {ae <- addExpr; ws; string ">="; ws; ce <- compareExpr; return (Op ">=" (Operators.>=) ae ce)}
compExp6 = do {ae <- addExpr; ws; string "<="; ws; ce <- compareExpr; return (Op "<=" (Operators.<=) ae ce)}


addExpr = addExp1 +++ addExp2 +++ multExpr
addExp1 = do {me <- multExpr; ws; char '+'; ws; ae <- addExpr; return (Op "+" (Operators.+) me ae)}
addExp2 = do {me <- multExpr; ws; char '-'; ws; ae <- addExpr; return (Op "-" (Operators.-) me ae)}


multExpr = multExp1 +++ multExp2 +++ negExpr
multExp1 = do {ne <- negExpr; ws; char '*'; ws; me <- multExpr; return (Op "*" (Operators.*) ne me)}
multExp2 = do {ne <- negExpr; ws; char '/'; ws; me <- multExpr; return (Op "/" (Operators./) ne me)}


negExpr = negExp +++ powExpr
negExp = do {char '-'; ws; pe <- powExpr; return (Negative pe)}


powExpr = powExp +++ value
powExp = do {v <- value; ws; char '^'; ws; pe <- powExpr; return (Op "^" (Operators.^) v pe)}


value = constant +++ val1 +++ variable
val1 = do {c <- char '('; ws; e <- expression; ws; char ')'; return e}


variable = array +++ letter'
array = do {name <- letter'; ws; char '('; ws; indices <- expressionList; ws; char ')'; return (Array name indices)}


function = func1 +++ func2
func1 =  do {str <- (string "INT"); ws; char '('; ws; e <- expression; ws; char ')'; return (Function str e) }
func2 = do {str <- (string "RND"); ws; char '('; ws; e <- expression; ws; char ')'; return (Function str e) }


constant = const1 +++ string'
const1 = do {i <- integer; return (Integer' i)}
