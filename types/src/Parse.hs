module Parse where

import Abs
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple

-- a simplified type for our parser
type Parser = Parsec Void String

parseterm :: String -> Either String Term
parseterm s = case parse pterm "" s of
  Right x -> Right x
  Left x -> Left $ errorBundlePretty x

-- a pretty term parser

prettyterm :: String -> IO ()
prettyterm s = case parse pterm "" s of
  Right x -> do
    putStrLn "\nGood news! The parser succeeded. Here's the abstract syntax:\n"
    pPrint x
    putStrLn ""
  Left x -> do putStrLn $ "\nBad news! The parser failed with the following message:\n\n" ++ (errorBundlePretty x) ++ "\n"

-- helpers

-- a parser that consume empty space
sc :: Parser ()
sc = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

-- makes a parser that parses a string and consumes empty space after it
symbol :: String -> Parser String
symbol = L.symbol sc

-- parse between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- parse between brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- parse between braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- symbols

dot :: Parser String
dot = symbol "."

colon :: Parser String
colon = symbol ":"

comma :: Parser String
comma = symbol ","

arrow :: Parser String
arrow = symbol "->" <|> symbol "→"

plus :: Parser String
plus = symbol "+"

times :: Parser String
times = symbol "*"

leftbrace :: Parser String
leftbrace = symbol "{"

rightbrace :: Parser String
rightbrace = symbol "}"

lambda :: Parser String
lambda = symbol "\\"

atype :: Parser String
atype = symbol "A"

btype :: Parser String
btype = symbol "B"

ctype :: Parser String
ctype = symbol "C"

dtype :: Parser String
dtype = symbol "D"

etype :: Parser String
etype = symbol "E"

ttype :: Parser String
ttype = symbol "T"

bottype :: Parser String
bottype = symbol "Bot"

fstsymb :: Parser String
fstsymb = symbol "fst"

sndsymb :: Parser String
sndsymb = symbol "snd"

inl :: Parser String
inl = symbol "inl"

inr :: Parser String
inr = symbol "inr"

as :: Parser String
as = symbol "as"

casesymb :: Parser String
casesymb = symbol "case"

ofsymb :: Parser String
ofsymb = symbol "of"

orsymb :: Parser String
orsymb = symbol "|"

abortsymb :: Parser String
abortsymb = symbol "abort"

-- parse type

ptype :: Parser Type
ptype =
  do
    try (parens psum)
    <|> try (parens pprod)
    <|> try (parens pfunc)
    <|> pbase
    <?> "a type here"

pbase :: Parser Type
pbase =
  try
    (Bot <$ bottype)
    <|> try (Base A <$ atype)
    <|> try
      (Base B <$ btype)
    <|> try
      (Base C <$ ctype)
    <|> try
      (Base D <$ dtype)
    <|> try
      (Base E <$ etype)

psum :: Parser Type
psum = Sum <$> ptype <* plus <*> ptype

pprod :: Parser Type
pprod = Prod <$> ptype <* times <*> ptype

pfunc :: Parser Type
pfunc = Func <$> ptype <* arrow <*> ptype

-- parse terms

pterm :: Parser Term
pterm =
  do try (parens pterm)
    <|> try (parens papp)
    <|> try pabs
    <|> try ppair
    <|> try pfst
    <|> try psnd
    <|> try pinl
    <|> try pinr
    <|> try pcase
    <|> try pabort
    <|> try (Var <$> pvar)
    <?> "a term here"

-- parse variables
pvar :: Parser VarSym
pvar = VarSym <$> alphaNumChar <*> try pindex

pindex :: Parser (Maybe String)
pindex = optional (some digitChar)

-- parse abstractions
pabs :: Parser Term
pabs = Abs <$ lambda <*> pvar <* colon <*> ptype <* dot <*> pterm

-- parse applications
papp :: Parser Term
papp = App <$> pterm <* sc <*> pterm

-- parse products
ppair :: Parser Term
ppair = Pair <$ leftbrace <*> pterm <* comma <*> pterm <* rightbrace

-- parse fst
pfst :: Parser Term
pfst = Fst <$ fstsymb <*> pterm

-- parse snd
psnd :: Parser Term
psnd = Snd <$ sndsymb <*> pterm

-- parse inl
pinl :: Parser Term
pinl = Inl <$ inl <*> pterm <* sc <* as <*> ptype

-- parse inr
pinr :: Parser Term
pinr = Inr <$ inr <*> pterm <* sc <* as <*> ptype

-- parse case
pcase :: Parser Term
pcase = Case <$ casesymb <*> pterm <* sc <* ofsymb <*> pterm <* sc <* orsymb <*> pterm

-- parse abort
pabort :: Parser Term
pabort = Abort <$ abortsymb <*> ptype <*> pterm
