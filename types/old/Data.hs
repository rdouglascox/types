module Data where

import qualified Data.Map as Map

-- here is the dataype for our types
data Type
  = E -- we write as `e`
  | T -- we write as `t`
  | Bot -- we write as `bot`
  | Func Type Type -- we write as `(t->t)`
  | Product Type Type -- we write as `(t x t)`
  | Sum Type Type -- we write as `(t +t)`
  deriving (Show, Eq)

-- here is the datatype for our variables
type VarName = String

-- here is the datatype for our terms
data Term
  = Var VarName -- we write as 'x1,x2,x3,...'
  | App Term Term -- we write as `(t1 t2)`
  | Abs VarName Type Term -- we write as `(\x:typ.t)`
  | Pair Term Term -- we write as `{t1,t2}`, will be of product type, corresponds to introduction rule for product
  | Fst Term -- we write as `fst t`, will be of the type of the first of a pair, corresponds to elimination rule for product
  | Snd Term -- we write as `snd t`, will be of the type of the second of a pair, corresponds to elimination rule for product
  | Inl Term Type -- we write as `inl t as typ`, will be the type of the left of a sum, corresponds to introduction rule for sum
  | Inr Term Type -- we write as `inr t as typ`, will be the type of the right of a sum, corresponds to introduction rule for sum
  | Case Term Term Term -- we write as `case t:typ of t1 | t2`, will be the type of the result of a case, corresponds to elimination rule for sum
  | Abort Type Term -- this is the elimination rule for bot, we write it as `abort typ t`
  deriving (Show, Eq)

-- here is the type for a typed term, which we write as `(t:typ)`
type TypedTerm = (Term, Type)

-- here is the type for a context, which we write as `{x1:typ1,x2:typ2,...}`
type Context = Map.Map String Type

-- here is the type for a judgement, which we write as `ctx |- (t:typ)`
type Judgement = (Context, TypedTerm)
