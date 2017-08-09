import Val_BASIC_parser
import Control.Monad.State
import Control.Applicative hiding (many)
import Parselib
import Data.List
import System.Random
import qualified Text.Read as R
import qualified Data.Map.Strict as Map
import qualified Val_BASIC_operators as Operators
import Debug.Trace
--------------------


type Globals = [(Expr,Expr)]


main = do
  program <- readFile "foo_bas.hs" -- input file name here
  let programLines = mapProgram program Map.empty
  let fstStmt = Map.lookupGE 0 programLines
  putStrLn $ show programLines
  runProgram fstStmt [] programLines []


mapProgram :: String -> Map.Map Int Stmt -> Map.Map Int Stmt
mapProgram "" map' = map'
mapProgram program map' = let lin = (runState extractLineVal $ program)
                          in  mapProgram (snd lin) (Map.insert (linNum $ fst lin) (stmt $ fst lin) map')
  

extractLineVal :: State String Stmt
extractLineVal = state $ \str -> head (parse line' str)

       
runProgram :: Maybe (Int, Stmt) -> Globals -> Map.Map Int Stmt -> [Int] -> IO()

runProgram (Just (_, END)) _ _ _=  return ()

runProgram (Just (lineNum, REM str)) globals programMap ret =  do
  putStrLn $ show str
  nextLine lineNum programMap globals ret
  
runProgram (Just (lineNum, INPUT cs is)) globals programMap ret =  do
  putStr ("? " ++ cs) -- prompt user for variable values
  strValues <- sequence $ replicate (length is) getLine -- translate prompt strings into expressions
  let exprValues = map (simplifyInput globals) strValues
  nextLine lineNum programMap ((zip is exprValues)++globals) ret
  
runProgram (Just (lineNum, PRINT ps)) globals programMap ret =  do
  putStrLn $ concatMap show (simplifyMap ps globals)
  nextLine lineNum programMap globals ret
 
runProgram (Just (lineNum, FOR index start stop step body)) globals programMap ret = do
  let map' = mapForLoop body
  let (result, str) =
        (forLoop index (simplify start globals) (simplify stop globals) (simplify step globals) map'
         (minKey $ map') (addIndexVal index start globals) [])
  putStrLn $ concatMap show str
  case (result) of
    (0, newGlobals) -> do
      nextLine lineNum programMap newGlobals ret
    (nextNum, newGlobals) -> do
      runProgram (Map.lookupLE nextNum programMap) newGlobals programMap ret

runProgram (Just (lineNum, GOSUB num)) globals programMap ret =  do
  runProgram (Map.lookupLE num programMap) globals programMap ((nextLineNum lineNum programMap):ret)
  
runProgram (Just(lineNum, stmt)) globals  programMap ret = case ((runState (evaluate stmt)) globals) of
            (-1, newGlobals) -> do
              runProgram (Map.lookupLE (head ret) programMap) newGlobals programMap (drop 1 ret)
            (0, newGlobals) -> do
              nextLine lineNum programMap newGlobals ret
            (nextNum, newGlobals) -> do
              runProgram (Map.lookupLE nextNum programMap) newGlobals programMap ret
              

nextLine lineNum map' globals ret = runProgram (Map.lookupGT lineNum map') globals map' ret

  
evaluate :: Stmt -> State Globals Int
evaluate (LET l r) = state $ \globals -> (0, (l,simplify r globals):globals)
evaluate (RETURN) = state $ \globals -> (-1, globals)
evaluate (GOTO num) = state $ \globals -> (num, globals)
evaluate (DIM arrays) = state $ \globals -> (0, globals)
evaluate (ONGOTO var lines) = state $ \globals -> (getIndex var lines globals, globals)
evaluate (IF bool (GOTO num)) = state $ \globals -> case (simplify bool globals) of
  (Bool' True) -> (num, globals)
  (Bool' False) -> (0, globals)

--buildArrays (Array name [a]) = [(name, Array ) (listArray (0,(integer' a) - 1) (replicate (integer' a - 1) 0)))
--buildArrays (Array name [a,b]) = (name, Arr2 name (listArray ((0,(integer' a)-1),(0,(integer' b)-1))(replicate ((integer' a - 1)*(integer' b - 1)) 0)))

--buildArrays (Array name [a]) = (name, Arr1 name (listArray (0,(integer' a) - 1) (replicate (integer' a - 1) 0)))
--buildArrays (Array name [a,b]) = (name, Arr2 name (listArray ((0,(integer' a)-1),(0,(integer' b)-1))(replicate ((integer' a - 1)*(integer' b - 1)) 0)))

getIndex v ls g = (ls !! (integer' (simplify v g)))

-- FOR Loop Logic
forLoop :: Expr -> Expr -> Expr -> Expr -> Map.Map Int Stmt -> Int -> Globals -> [Expr]-> ((Int, Globals), [Expr])
forLoop index current stop step forMap lineNum globals output
  | (completedLoop current stop) = ((0, delete (index,current) globals), output)
  | (jumpOutLoop lineNum forMap) = ((lineNum, delete (index,current) globals), output)
  | otherwise = let stmt = (nextStmt lineNum forMap)
                in case stmt of
      (PRINT exprs) ->
        forLoop index current stop step forMap (nextLineNum lineNum forMap) globals
          (loopPrint exprs globals output)
      (NEXT [i]) ->
        case (index `elem` [i]) of
          True -> 
            forLoop index (current Operators.+ step) stop step forMap (nextLineNum lineNum forMap)
              (addIndexVal index (current Operators.+ step) globals) output
          False -> 
            forLoop index current stop step forMap (nextLineNum lineNum forMap) globals output
      (otherStmt) ->
        case (runState (evaluate otherStmt) globals) of
          (0, newGlobals) ->
            forLoop index current stop step forMap (nextLineNum lineNum forMap) newGlobals output
          (num, newGlobals) ->
            forLoop index current stop step forMap num newGlobals output
    

loopPrint :: [Expr] -> Globals -> [Expr] -> [Expr]
loopPrint exprs globals output = output ++ (simplifyMap exprs globals)
        
nextStmt :: Int -> Map.Map Int Stmt -> Stmt
nextStmt int map' = abstractValue $ Map.lookupGE int map'

completedLoop :: Expr -> Expr -> Bool
completedLoop cur stop = bool' (cur Operators.> stop)

jumpOutLoop :: Int -> Map.Map Int Stmt -> Bool
jumpOutLoop num map' = (not (Map.member num map'))

addIndexVal :: Expr -> Expr -> Globals -> Globals
addIndexVal index value globals = (index, value):[ (var,val) | (var,val) <- globals, var /= index]

nextLineNum :: Int -> Map.Map Int Stmt -> Int
nextLineNum num map' = (abstractKey (Map.lookupGT num map') map')

abstractKey (Just (key, value)) _  = key
abstractKey Nothing map' = minKey map'

abstractValue (Just (key,value)) = value

minKey map' = fst $ Map.findMin map'

mapForLoop :: [Stmt] ->  Map.Map Int Stmt
mapForLoop body  = Map.fromList [(i,s) | (LINE i s) <- body]



-- Simplify Expressions
simplify :: Expr -> Globals -> Expr
simplify (Op "+" (+) a b) globals = (Operators.+) (simplify a globals) (simplify b globals)
simplify (Op "-" (-) a b) globals = (Operators.-) (simplify a globals) (simplify b globals)
simplify (Op "*" (*) a b) globals = (Operators.*) (simplify a globals) (simplify b globals)
simplify (Op "/" (/) a b) globals = (Operators./) (simplify a globals) (simplify b globals)
simplify (Op "^" (^) a b) globals = (Operators.^) (simplify a globals) (simplify b globals)

simplify (Op "=" (==) a b) globals = (Operators.==) (simplify a globals) (simplify b globals)
simplify (Op ">" (>) a b) globals = (Operators.>) (simplify a globals) (simplify b globals)
simplify (Op "<" (<) a b) globals = (Operators.<) (simplify a globals) (simplify b globals)
simplify (Op "<>" (<>) a b) globals = (Operators.<>) (simplify a globals) (simplify b globals)
simplify (Op ">=" (>=) a b) globals = (Operators.>=) (simplify a globals) (simplify b globals)
simplify (Op "<=" (<=) a b) globals = (Operators.<=) (simplify a globals) (simplify b globals)

simplify (Op "OR" _ a b) globals = (Operators.or') (simplify a globals) (simplify b globals)
simplify (Op "AND" _ a b) globals = (Operators.and') (simplify a globals) (simplify b globals)

simplify (Function "INT" expr) globals = Integer' (ceiling (float' (simplify expr globals)))
simplify (Function "RND" expr) globals = Float' $ fst ((runState ranNum) (mkStdGen $ integer' (simplify expr globals)))

simplify (Negative a) globals = Integer' (- integer' (simplify a globals))
simplify (Grouped e) globals = simplify e globals
simplify (Integer' a) globals = (Integer' a)
simplify (Float' a) globals = (Float' a)
simplify (Id a) globals = head $ findGlobal [(Id a)] globals
simplify (String' s) globals = head $ findGlobal [(String' s)] globals
simplify (Array n i) globals = head $ findGlobal [(Array n i)] globals

simplifyMap :: [Expr] -> Globals -> [Expr]
simplifyMap exprs globals = map (`simplify` globals)  exprs

simplifyInput :: Globals -> String -> Expr
simplifyInput global var = simplify (fst $ head (parse expression var)) global

findGlobal :: [Expr] -> Globals -> [Expr]
findGlobal [] _ = []
findGlobal (p:ps) globals =  case lookup p globals of
  Nothing -> p : findGlobal ps globals
  Just x -> x : findGlobal ps globals

ranNum :: State StdGen Float
ranNum = state $ random 
