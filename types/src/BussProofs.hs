module BussProofs (printbussproof) where

import Abs
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree

-- printing functions

printbussproof xs = concat $ intersperse "\n" $ reverse $ printderivation xs

printderivation :: Derivation -> [String]
printderivation (Derivation ctx trm mtyp mrule []) = ["\\AxiomC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"]
printderivation (Derivation ctx trm mtyp mrule [x]) = ["\\UnaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ (printderivation x)
printderivation (Derivation ctx trm mtyp mrule [x, y]) = ["\\BinaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ (printderivation x) ++ (printderivation y)
printderivation (Derivation ctx trm mtyp mrule [x, y, z]) = ["\\TernaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ (printderivation x) ++ (printderivation y) ++ (printderivation z)

printmtmr :: Maybe Type -> Maybe Rule -> String
printmtmr t r = case (t, r) of
  (Just x, Just y) -> printtype x
  _ -> "error"

-- the latex for an empty set is \emptyset
-- the latex for gamma is \Gamma
printcontext :: Context -> String
printcontext ctx = case Map.toList ctx of
  [] -> "$\\emptyset$"
  xs -> "$\\Gamma$, " ++ intercalate ", " (map pmap xs)

pmap :: (VarSym, Type) -> String
pmap (VarSym x1 x2, y) = case x2 of
  Just x3 -> x1 : x3 ++ ":" ++ printtype y
  Nothing -> [x1] ++ ":" ++ printtype y

printtype :: Type -> String
printtype typ = case typ of
  Base x -> show x
  Func x y -> "(" ++ printtype x ++ "$\\to$" ++ printtype y ++ ")"
  Sum x y -> "(" ++ printtype x ++ "+" ++ printtype y ++ ")"
  Prod x y -> "(" ++ printtype x ++ "$\\times$" ++ printtype y ++ ")"
  Bot -> "$\\bot$"

printterm :: Term -> String
printterm trm = case trm of
  App trm1 trm2 -> "(" ++ printterm trm1 ++ " " ++ printterm trm2 ++ ")"
  Abs var1 typ1 trm1 -> "$\\lambda$" ++ printvarsym var1 ++ ":" ++ printtype typ1 ++ "." ++ printterm trm1
  Pair trm1 trm2 -> "\\{" ++ printterm trm1 ++ "," ++ printterm trm2 ++ "\\}"
  Snd trm1 -> "(" ++ "snd " ++ printterm trm1 ++ ")"
  Fst trm1 -> "(" ++ "fst " ++ printterm trm1 ++ ")"
  Inl trm1 typ -> "(" ++ "inl " ++ printterm trm1 ++ " as " ++ printtype typ ++ ")"
  Inr trm1 typ -> "(" ++ "inr " ++ printterm trm1 ++ " as " ++ printtype typ ++ ")"
  Case trm1 trm2 trm3 -> "(" ++ "case " ++ printterm trm1 ++ " of " ++ printterm trm2 ++ " | " ++ printterm trm3 ++ ")"
  Abort typ trm1 -> "(" ++ "abort" ++ printterm trm1 ++ printtype typ ++ ")"
  Var var -> printvarsym var

printvarsym :: VarSym -> String
printvarsym (VarSym x x1) = case x1 of
  Just x2 -> x : x2
  Nothing -> [x]
