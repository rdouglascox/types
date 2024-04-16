module Parsing where

import Abs
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char

-- functions for running the parsers
-- parsing functions.

parsejudgment :: Parsec String () (Context, Term, Type)
parsejudgment = do
  ctx1 <- parsecontext
  string "|-"
  trm1 <- parseterm
  char ':'
  typ1 <- parsetype
  return (ctx1, trm1, typ1)

-- we write a context as a list of variables and types,
-- like this {x:T, y:(T->E)} and so on.
-- we are expeding a list of bindings separated by commas
parsecontext :: Parsec String () Context
parsecontext = do
  char '{'
  bindings <- sepBy parsebinding (char ',')
  char '}'
  return (foldr (Map.union) Map.empty bindings)

parsebinding :: Parsec String () (Map.Map VarSym Type)
parsebinding = do
  str1 <- parsevar
  char ':'
  typ1 <- parsetype
  return (Map.singleton str1 typ1)

-- for parsing types we had better use try
-- and we want to fail if we can't parse all the way to whitespace
parsetype :: Parsec String () Type
parsetype = try parsesumtype <|> try parseproducttype <|> try parsefunctype <|> try parsebasetype

parsesumtype :: Parsec String () Type
parsesumtype = do
  char '('
  t1 <- parsetype
  char '+'
  t2 <- parsetype
  char ')'
  return (Sum t1 t2)

parseproducttype :: Parsec String () Type
parseproducttype = do
  char '('
  t1 <- parsetype
  char 'x'
  t2 <- parsetype
  char ')'
  return (Prod t1 t2)

parsefunctype :: Parsec String () Type
parsefunctype = do
  char '('
  t1 <- parsetype
  string "->"
  t2 <- parsetype
  char ')'
  return (Func t1 t2)

-- we need to add try to this
parsebasetype :: Parsec String () Type
parsebasetype = try (do char 'E'; return E) <|> try (do char 'T'; return T) <|> try (do string "F"; return Bot)

parseannotatedterm :: Parsec String () (Term, Type)
parseannotatedterm = do
  trm1 <- parseterm
  char ':'
  typ1 <- parsetype
  return (trm1, typ1)

-- we need to use try here
parseterm :: Parsec String () Term
parseterm =
  try parseapp
    <|> try parseabs
    <|> try parseproduct
    <|> try parseinl
    <|> try parseinr
    <|> try parsefst
    <|> try parsesnd
    <|> try parsecase
    <|> try parseabort
    <|> try parsevar

-- applications must be surrounded by parentheses, and the whitespace is required
parseapp :: Parsec String () Term
parseapp = do
  char '('
  trm1 <- parseterm
  _ <- whitespace
  trm2 <- parseterm
  char ')'
  return (App trm1 trm2)

whitespace :: Parsec String () ()
whitespace = skipMany (char ' ')

parseabs :: Parsec String () Term
parseabs = do
  char '\\'
  str1 <- parsevar
  char ':'
  typ1 <- parsetype
  char '.'
  trm1 <- parseterm
  return (Abs str1 typ1 trm1)

parsevar :: Parsec String () Term
parsevar = do
  str1 <- lowerChar
  return (Var (VarSym str1 Nothing))

parseproduct :: Parsec String () Term
parseproduct = do
  char '{'
  trm1 <- parseterm
  char ','
  trm2 <- parseterm
  char '}'
  return (Pair trm1 trm2)

parseinl :: Parsec String () Term
parseinl = do
  string "inl"
  _ <- whitespace
  trm1 <- parseterm
  _ <- whitespace
  string "as"
  _ <- whitespace
  typ1 <- parsetype
  return (Inl trm1 typ1)

parseinr :: Parsec String () Term
parseinr = do
  string "inr"
  _ <- whitespace
  trm1 <- parseterm
  _ <- whitespace
  string "as"
  _ <- whitespace
  typ1 <- parsetype
  return (Inr trm1 typ1)

parsefst :: Parsec String () Term
parsefst = do
  string "fst"
  _ <- whitespace
  trm1 <- parseterm
  return (Fst trm1)

parsesnd :: Parsec String () Term
parsesnd = do
  string "snd"
  _ <- whitespace
  trm1 <- parseterm
  return (Snd trm1)

parsecase :: Parsec String () Term
parsecase = do
  string "case"
  _ <- whitespace
  trm1 <- parseterm
  _ <- whitespace
  string "of"
  _ <- whitespace
  trm2 <- parseterm
  _ <- whitespace
  string "|"
  _ <- whitespace
  trm3 <- parseterm
  _ <- whitespace
  return (Case trm1 trm2 trm3)

parseabort :: Parsec String () Term
parseabort = do
  string "abort"
  typ1 <- parsetype
  trm1 <- parseterm
  return (Abort typ1 trm1)
