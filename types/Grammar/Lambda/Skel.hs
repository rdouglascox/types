-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Lambda.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Lambda.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transVar :: Lambda.Abs.Var -> Result
transVar x = case x of
  Lambda.Abs.Var string -> failure x

transTerm :: Lambda.Abs.Term -> Result
transTerm x = case x of
  Lambda.Abs.Variable var -> failure x
  Lambda.Abs.Application term1 term2 -> failure x
  Lambda.Abs.Abstraction var type_ term -> failure x
  Lambda.Abs.Fst term -> failure x
  Lambda.Abs.Snd term -> failure x
  Lambda.Abs.Inl term type_ -> failure x
  Lambda.Abs.Inr term type_ -> failure x
  Lambda.Abs.Case term1 term2 term3 -> failure x
  Lambda.Abs.Abort type_ term -> failure x

transType :: Lambda.Abs.Type -> Result
transType x = case x of
  Lambda.Abs.A -> failure x
  Lambda.Abs.B -> failure x
  Lambda.Abs.C -> failure x
  Lambda.Abs.T -> failure x
  Lambda.Abs.E -> failure x
  Lambda.Abs.Bot -> failure x
  Lambda.Abs.Func type_1 type_2 -> failure x
  Lambda.Abs.Sum type_1 type_2 -> failure x
  Lambda.Abs.Product type_1 type_2 -> failure x
