module Abs where

import qualified Data.Map as Map

data BaseType = A | B | C | D | E | T
  deriving (Show, Eq)

data Type
  = Base BaseType
  | Bot
  | Func Type Type
  | Sum Type Type
  | Prod Type Type
  deriving (Show, Eq)

data VarSym = VarSym Char (Maybe String)
  deriving (Show, Eq, Ord)

data Term
  = Var VarSym
  | Abs VarSym Type Term
  | App Term Term
  | Pair Term Term
  | Fst Term
  | Snd Term
  | Inl Term Type
  | Inr Term Type
  | Case Term Term Term
  | Abort Type Term
  deriving (Show, Eq)

type Context = Map.Map VarSym Type

data Rule
  = VarRule
  | AbsRule
  | AppRule
  | PairRule
  | FstRule
  | SndRule
  | InlRule
  | InrRule
  | CaseRule
  | AbortRule
  deriving (Eq)

instance Show Rule where
  show x = case x of
    VarRule -> "[Ax]"
    AbsRule -> "[→I]"
    AppRule -> "[→E]"
    PairRule -> "[×I]"
    FstRule -> "[×E₁]"
    SndRule -> "[×E₂]"
    InlRule -> "[+I₁]"
    InrRule -> "[+I₂]"
    CaseRule -> "[+E]"
    AbortRule -> "[⊥ E]"

data Derivation = Derivation Context Term (Maybe Type) (Maybe Rule) [Derivation]
  deriving (Show)

data DerivationErr = DerivationErr Context Term (Either String Type) (Maybe Rule) [DerivationErr]
  deriving (Show)
