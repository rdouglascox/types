module Printing (printjudgement, printtype) where

import Data
import Data.List
import qualified Data.Map as Map
import Data.Tree

-- printing functions

printjudgement :: Judgement -> String
printjudgement (ctx, (trm1, typ1)) = printcontext ctx ++ " |- " ++ printterm trm1 ++ ":" ++ printtype typ1

printcontext :: Context -> String
printcontext ctx = case Map.toList ctx of
  [] -> "null"
  xs -> intercalate ", " (map pmap xs)

pmap :: (String, Type) -> String
pmap (x, y) = x ++ ":" ++ printtype y

printtype :: Type -> String
printtype typ = case typ of
  E -> "e"
  T -> "t"
  Func x y -> "(" ++ printtype x ++ "->" ++ printtype y ++ ")"
  Sum x y -> "(" ++ printtype x ++ "+" ++ printtype y ++ ")"
  Product x y -> "(" ++ printtype x ++ "x" ++ printtype y ++ ")"
  Bot -> "f"

printterm :: Term -> String
printterm trm = case trm of
  App trm1 trm2 -> "(" ++ printterm trm1 ++ ")" ++ printterm trm2
  Abs var1 typ1 trm1 -> "\\" ++ var1 ++ ":" ++ printtype typ1 ++ "." ++ printterm trm1
  Pair trm1 trm2 -> "{" ++ printterm trm1 ++ ":" ++ printterm trm2 ++ "}"
  Snd trm1 -> "snd " ++ printterm trm1
  Fst trm1 -> "fst " ++ printterm trm1
  Inl trm1 typ -> "inl " ++ printterm trm1 ++ " as " ++ printtype typ
  Inr trm1 typ -> "inr " ++ printterm trm1 ++ " as " ++ printtype typ
  Case trm1 trm2 trm3 -> "case " ++ printterm trm1 ++ " of " ++ printterm trm2 ++ " | " ++ printterm trm3
  Abort typ trm1 -> "abort" ++ printterm trm1 ++ printtype typ
  Var str1 -> str1

jtree2stree :: Tree Judgement -> Tree String
jtree2stree = fmap printjudgement
