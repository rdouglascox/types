module BussProofs (printbussproof, prettybussproofs, prettybussproofs1) where

import Abs
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree

prettybussproofs1 :: [Derivation] -> String
prettybussproofs1 ds =
  "\\documentclass{article}\n"
    ++ "\\usepackage{bussproofs}\n"
    ++ "\\usepackage[landscape, margin=1cm]{geometry}\n"
    ++ "\\begin{document}\n"
    ++ concatMap prettybussproofs ds
    ++ "\\end{document}"

prettybussproofs :: Derivation -> String
prettybussproofs d@(Derivation _ trm mtyp _ _) =
  let (pterm, mptype) = (trm, mtyp)
   in "\\noindent Here is a derivation of: "
        ++ printterm pterm
        ++ ":"
        ++ case mptype of
          Just x -> printtype x
          Nothing -> "error"
        ++ "\n"
        ++ "\\begin{prooftree}"
        ++ printbussproof d
        ++ "\\end{prooftree}\n"
        ++ "Here is the cooresponding natural deduction proof:\n"
        ++ "\\begin{prooftree}"
        ++ printbussproof2 d
        ++ "\\end{prooftree}\n\n"
        ++ "\\hrule\\vspace{1cm}\n"

-- printing functions

printbussproof2 xs = concat $ intersperse "\n" $ reverse $ printderivation2 xs

printbussproof xs = concat $ intersperse "\n" $ reverse $ printderivation1 xs

printderivation :: Derivation -> [String]
printderivation (Derivation ctx trm mtyp mrule []) =
  ["\\AxiomC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"]
printderivation (Derivation ctx trm mtyp mrule [x]) =
  ["\\UnaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ (printderivation x)
printderivation (Derivation ctx trm mtyp mrule [x, y]) =
  ["\\BinaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ (printderivation x) ++ (printderivation y)
printderivation (Derivation ctx trm mtyp mrule [x, y, z]) =
  ["\\TrinaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ (printderivation x) ++ (printderivation y) ++ (printderivation z)

printderivation1 :: Derivation -> [String]
printderivation1 (Derivation ctx trm mtyp mrule []) =
  ["\\AxiomC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}", "\\RightLabel{" ++ printmrule mrule ++ "}"]
printderivation1 (Derivation ctx trm mtyp mrule [x]) =
  ["\\UnaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ ["\\RightLabel{" ++ printmrule mrule ++ "}"] ++ (printderivation1 x)
printderivation1 (Derivation ctx trm mtyp mrule [x, y]) =
  ["\\BinaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ ["\\RightLabel{" ++ printmrule mrule ++ "}"] ++ (printderivation1 x) ++ (printderivation1 y)
printderivation1 (Derivation ctx trm mtyp mrule [x, y, z]) =
  ["\\TrinaryInfC{" ++ (printcontext ctx ++ " $\\vdash$ " ++ printterm trm ++ ":" ++ printmtmr mtyp mrule) ++ "}"] ++ ["\\RightLabel{" ++ printmrule mrule ++ "}"] ++ (printderivation1 x) ++ (printderivation1 y) ++ (printderivation1 z)

printderivation2 :: Derivation -> [String]
printderivation2 (Derivation ctx trm mtyp mrule []) =
  ["\\AxiomC{" ++ printmtmrnd mtyp mrule ++ "}", "\\RightLabel{" ++ printmrule2 mrule ++ "}"]
printderivation2 (Derivation ctx trm mtyp mrule [x]) =
  ["\\UnaryInfC{" ++ printmtmrnd mtyp mrule ++ "}"] ++ ["\\RightLabel{" ++ printmrule2 mrule ++ "}"] ++ (printderivation2 x)
printderivation2 (Derivation ctx trm mtyp mrule [x, y]) =
  ["\\BinaryInfC{" ++ printmtmrnd mtyp mrule ++ "}"] ++ ["\\RightLabel{" ++ printmrule2 mrule ++ "}"] ++ (printderivation2 x) ++ (printderivation2 y)
printderivation2 (Derivation ctx trm mtyp mrule [x, y, z]) =
  ["\\TrinaryInfC{" ++ printmtmrnd mtyp mrule ++ "}"] ++ ["\\RightLabel{" ++ printmrule2 mrule ++ "}"] ++ (printderivation2 x) ++ (printderivation2 y) ++ (printderivation2 z)

printmrule2 :: Maybe Rule -> String
printmrule2 r = case r of
  Just x -> printrule2 x
  Nothing -> "error"

printrule2 :: Rule -> String
printrule2 r = case r of
  VarRule -> "{\\small Ax}"
  AbsRule -> "{\\small $\\to$Intro}"
  AppRule -> "{\\small $\\to$Elim}"
  PairRule -> "{\\small $\\land$Intro}"
  FstRule -> "{\\small $\\land$Elim1}"
  SndRule -> "{\\small $\\land$Elim2}"
  InlRule -> "{\\small $\\lor$Intro2}"
  InrRule -> "{\\small $\\lor$Intro1}"
  CaseRule -> "{\\small $\\lor$Elim}"
  AbortRule -> "{\\small $\\bot$Elim}"

printmrule :: Maybe Rule -> String
printmrule r = case r of
  Just x -> printrule x
  Nothing -> "error"

printrule :: Rule -> String
printrule r = case r of
  VarRule -> "{\\small Ax}"
  AbsRule -> "{\\small $\\to$Intro}"
  AppRule -> "{\\small $\\to$Elim}"
  PairRule -> "{\\small $\\times$Intro}"
  FstRule -> "{\\small $\\times$Elim1}"
  SndRule -> "{\\small $\\times$Elim2}"
  InlRule -> "{\\small +Intro2}"
  InrRule -> "{\\small +Intro1}"
  CaseRule -> "{\\small +Elim}"
  AbortRule -> "{\\small $\\bot$Elim}"

printmtmr :: Maybe Type -> Maybe Rule -> String
printmtmr t r = case (t, r) of
  (Just x, Just y) -> printtype x
  _ -> "error"

printmtmrnd :: Maybe Type -> Maybe Rule -> String
printmtmrnd t r = case (t, r) of
  (Just x, Just y) -> printtypemd x
  _ -> "error"

-- the latex for an empty set is \emptyset
-- the latex for gamma is \Gamma
printcontext :: Context -> String
printcontext ctx = case Map.toList ctx of
  [] -> "$\\emptyset$"
  xs -> "$\\Gamma$, " ++ intercalate ", " (map pmap xs)

pmap :: (VarSym, Type) -> String
pmap (VarSym x1 x2, y) = case x2 of
  Just x3 -> "\\textit{" ++ [x1] ++ "}" ++ "\\textsubscript{" ++ x3 ++ "}" ++ ":" ++ printtype y
  Nothing -> "\\textit{" ++ [x1] ++ "}" ++ ":" ++ printtype y

printtype :: Type -> String
printtype typ = case typ of
  Base x -> show x
  Func x y -> "(" ++ printtype x ++ "$\\to$" ++ printtype y ++ ")"
  Sum x y -> "(" ++ printtype x ++ "+" ++ printtype y ++ ")"
  Prod x y -> "(" ++ printtype x ++ "$\\times$" ++ printtype y ++ ")"
  Bot -> "$\\bot$"

printtypemd :: Type -> String
printtypemd typ = case typ of
  Base x -> show x
  Func x y -> "(" ++ printtypemd x ++ "$\\to$" ++ printtypemd y ++ ")"
  Sum x y -> "(" ++ printtypemd x ++ "$\\lor$" ++ printtypemd y ++ ")"
  Prod x y -> "(" ++ printtypemd x ++ "$\\land$" ++ printtypemd y ++ ")"
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
  Case trm1 trm2 trm3 -> "(" ++ "case " ++ printterm trm1 ++ " of " ++ printterm trm2 ++ " $\\vert$ " ++ printterm trm3 ++ ")"
  Abort typ trm1 -> "(" ++ "abort" ++ printterm trm1 ++ printtype typ ++ ")"
  Var var -> printvarsym var

printvarsym :: VarSym -> String
printvarsym (VarSym x x1) = case x1 of
  Just x2 -> "\\textit{" ++ [x] ++ "}" ++ "\\textsubscript{" ++ x2 ++ "}"
  Nothing -> "\\textit{" ++ [x] ++ "}"
