module TypeChecking where

import qualified Data.Map as Map

import Abs
import Parse
import PrintingUnicode (printderivation1, printtype)

typecheck :: Context -> Term -> Maybe Type
typecheck ctx intrm = case intrm of
  Var vsymb -> Map.lookup vsymb ctx
  Abs vsymb typ trm ->
    let newctx = Map.insert vsymb typ ctx
        bdytrmtyp = typecheck newctx trm
     in Func <$> Just typ <*> bdytrmtyp
  App trm1 trm2 ->
    let trm1typ = typecheck ctx trm1
        trm2typ = typecheck ctx trm2
     in case trm1typ of
          Just (Func ityp otyp) ->
            if Just ityp == trm2typ
              then Just otyp
              else Nothing
          _ -> Nothing
  Pair trm1 trm2 ->
    let trm1typ = typecheck ctx trm1
        trm2typ = typecheck ctx trm2
     in Prod <$> trm1typ <*> trm2typ
  Fst trm ->
    case typecheck ctx trm of
      Just (Prod typ1 _) -> Just typ1
      _ -> Nothing
  Snd trm ->
    case typecheck ctx trm of
      Just (Prod _ typ2) -> Just typ2
      _ -> Nothing
  Inl trm typ -> case typecheck ctx trm of
    Just typ1 ->
      if typ == typ1
        then Just typ
        else Nothing
    _ -> Nothing
  Inr trm typ -> case typecheck ctx trm of
    Just typ1 ->
      if typ == typ1
        then Just typ
        else Nothing
    _ -> Nothing
  Case trm1 trm2 trm3 -> case (typecheck ctx trm1, typecheck ctx trm2, typecheck ctx trm3) of
    (Just (Sum typl typr), Just (Func intypl outtypl), Just (Func intypr outtypr)) ->
      if (typl == intypl) && (typr == intypr) && (outtypl == outtypr)
        then Just outtypl
        else Nothing
    _ -> Nothing
  Abort typ trm -> case typecheck ctx trm of
    Just _ -> Just typ
    _ -> Nothing

typecheck1 :: Term -> Maybe Type
typecheck1 = typecheck Map.empty

typecheck2 :: String -> String
typecheck2 s = case mytermparser s of
  Right trm -> case typecheck1 trm of
    Just typ -> "The term is well-typed on the empty context. It has the type: " ++ mytypeprinter typ
    Nothing -> "The term is not well-typed."
  Left err -> "The parser failed with the following error message: " ++ err
 where
  mytermparser = parseterm
  mytypeprinter = printtype

td :: Derivation -> (Maybe Type, Derivation)
td d@(Derivation _ _ x _ _) = (x, d)

wd :: Context -> Term -> Derivation
wd ctx trm = Derivation ctx trm Nothing Nothing []

derive :: Derivation -> Derivation
derive (Derivation ctx intrm _ _ _) = case intrm of
  Var vsymb -> Derivation ctx intrm (Map.lookup vsymb ctx) (Just VarRule) []
  Abs vsymb typ trm ->
    let newctx = Map.insert vsymb typ ctx
        (ntyp, d1) = td $ derive $ wd newctx trm
     in wrap (Func <$> Just typ <*> ntyp) (Just AbsRule) [d1]
  App trm1 trm2 ->
    let (trm1typ, d1) = td $ derive $ wd ctx trm1
        (trm2typ, d2) = td $ derive $ wd ctx trm2
     in case trm1typ of
          Just (Func ityp otyp) ->
            if Just ityp == trm2typ
              then wrap (Just otyp) (Just AppRule) [d1, d2]
              else wrap Nothing Nothing [d1, d2]
          _ -> wrap Nothing Nothing [d1, d2]
  Pair trm1 trm2 ->
    let (trm1typ, d1) = td $ derive $ wd ctx trm1
        (trm2typ, d2) = td $ derive $ wd ctx trm2
     in wrap (Prod <$> trm1typ <*> trm2typ) (Just PairRule) [d1, d2]
  Fst trm ->
    let (typ, d1) = td $ derive $ wd ctx trm
     in case typ of
          Just (Prod typ1 _) -> wrap (Just typ1) (Just FstRule) [d1]
          _ -> wrap Nothing Nothing [d1]
  Snd trm ->
    let (typ, d1) = td $ derive $ wd ctx trm
     in case typ of
          Just (Prod _ typ2) -> wrap (Just typ2) (Just SndRule) [d1]
          _ -> wrap Nothing Nothing [d1]
  Inl trm typ ->
    let (typx, d1) = td $ derive $ wd ctx trm
     in case typx of
          Just typ1 ->
            if typ == typ1
              then wrap (Just typ) (Just InlRule) [d1]
              else wrap Nothing Nothing [d1]
          _ -> wrap Nothing Nothing [d1]
  Inr trm typ ->
    let (typx, d1) = td $ derive $ wd ctx trm
     in case typx of
          Just typ1 ->
            if typ == typ1
              then wrap (Just typ) (Just InrRule) [d1]
              else wrap Nothing Nothing [d1]
          _ -> wrap Nothing Nothing [d1]
  Case trm1 trm2 trm3 ->
    let (typ1x, d1) = td $ derive $ wd ctx trm1
        (typ2x, d2) = td $ derive $ wd ctx trm2
        (typ3x, d3) = td $ derive $ wd ctx trm3
     in case (typ1x, typ2x, typ3x) of
          (Just (Sum typl typr), Just (Func intypl outtypl), Just (Func intypr outtypr)) ->
            if (typl == intypl) && (typr == intypr) && (outtypl == outtypr)
              then wrap (Just outtypl) (Just CaseRule) [d1, d2, d3]
              else wrap Nothing Nothing [d1, d2, d2]
          _ -> wrap Nothing Nothing [d1, d2, d2]
  Abort typ trm ->
    let (typ1, d1) = td $ derive $ wd ctx trm
     in case typ1 of
          Just _ -> wrap (Just typ) (Just AbortRule) [d1]
          _ -> wrap Nothing Nothing [d1]
 where
  wrap :: Maybe Type -> Maybe Rule -> [Derivation] -> Derivation
  wrap = Derivation ctx intrm

derive1 :: String -> String
derive1 s = case mytermparser s of
  Right trm -> "\n" ++ printderivation1 (derive (Derivation Map.empty trm Nothing Nothing []))
  Left err -> "\nThe parser failed with the following error message: " ++ err
 where
  mytermparser = parseterm

test1 = derive1 "\\x:(A*B).{snd x,fst x}"
