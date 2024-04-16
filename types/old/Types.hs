module Types where

import Data
import qualified Data.Map as Map
import Data.Tree (Tree (..), drawTree)
import Parsing
import Printing (printjudgement)
import PrintingTrees
import PrintingUnicode (printderivation, printjudgementU, printterm, printtype)
import Text.Parsec (parse)

testdev :: String -> String -> String
testdev s1 s2 = case (parse parsecontext "" s1, parse parseterm "" s2) of
  (Right ctx, Right trm) -> drawVerticalTreeBU (fmap printderivation (todev ctx trm))
  _ -> "something went wrong"

todev :: Context -> Term -> Tree (Context, Term, Maybe Type)
todev ctx trm = derivation (Node (ctx, trm, Nothing) [])

fromdev :: Tree (Context, Term, Maybe Type) -> Maybe Type
fromdev (Node (_, _, mtype) _) = mtype

derivation :: Tree (Context, Term, Maybe Type) -> Tree (Context, Term, Maybe Type)
derivation (Node (ctx, trm, _) _) = case trm of
  Var x -> case Map.lookup x ctx of
    Just typ -> Node (ctx, trm, Just typ) []
    _ -> Node (ctx, trm, Nothing) []
  Abs x vtyp btrm ->
    let g = derivation (Node (Map.insert x vtyp ctx, btrm, Nothing) [])
     in case fromdev g of
          Just dtyp -> Node (ctx, trm, Just (Func vtyp dtyp)) [g]
          _ -> Node (ctx, trm, Nothing) [g]
  App t1 t2 ->
    let g1 = derivation (Node (ctx, t1, Nothing) [])
        g2 = derivation (Node (ctx, t2, Nothing) [])
     in case (fromdev g1, fromdev g2) of
          (Just (Func typ1 typ2), Just typ3) ->
            if typ1 == typ3
              then Node (ctx, trm, Just typ2) [g1, g2]
              else Node (ctx, trm, Nothing) [g1, g2]
          _ -> Node (ctx, trm, Nothing) [g1, g2]
  Pair t1 t2 ->
    let g1 = derivation (Node (ctx, t1, Nothing) [])
        g2 = derivation (Node (ctx, t2, Nothing) [])
     in case (fromdev g1, fromdev g2) of
          (Just typ1, Just typ2) -> Node (ctx, trm, Just (Product typ1 typ2)) [g1, g2]
          _ -> Node (ctx, trm, Nothing) [g1, g2]
  Fst t ->
    let g = derivation (Node (ctx, t, Nothing) [])
     in case fromdev g of
          Just (Product typ1 _) -> Node (ctx, trm, Just typ1) [g]
          _ -> Node (ctx, trm, Nothing) [g]
  Snd t ->
    let g = derivation (Node (ctx, t, Nothing) [])
     in case fromdev g of
          Just (Product _ typ2) -> Node (ctx, trm, Just typ2) [g]
          _ -> Node (ctx, trm, Nothing) [g]
  Inl t typ1 ->
    let g = derivation (Node (ctx, t, Nothing) [])
     in case (fromdev g, typ1) of
          (Just typ2, Sum stl _) -> if typ2 == stl then Node (ctx, trm, Just typ1) [g] else Node (ctx, trm, Nothing) [g]
          _ -> Node (ctx, trm, Nothing) [g]
  Inr t typ2 ->
    let g = derivation (Node (ctx, t, Nothing) [])
     in case (fromdev g, typ2) of
          (Just typ1, Sum _ str) -> if typ1 == str then Node (ctx, trm, Just typ2) [g] else Node (ctx, trm, Nothing) [g]
          _ -> Node (ctx, trm, Nothing) [g]
  Case t t1 t2 ->
    let g = derivation (Node (ctx, t, Nothing) [])
     in case fromdev g of
          Just (Sum typ1 typ2) ->
            let g1 = derivation (Node (ctx, t1, Nothing) [])
                g2 = derivation (Node (ctx, t2, Nothing) [])
             in case (fromdev g1, fromdev g2) of
                  (Just (Func typin1 typeout1), Just (Func typein2 typeout2)) ->
                    if typ1 == typin1 && typ2 == typein2 && typeout1 == typeout2
                      then Node (ctx, trm, Just typeout2) [g, g1, g2]
                      else Node (ctx, trm, Nothing) [g, g1, g2]
                  _ -> Node (ctx, trm, Nothing) [g, g1, g2]
          _ -> Node (ctx, trm, Nothing) [g]
  Abort typ1 t ->
    let g = derivation (Node (ctx, t, Nothing) [])
     in case fromdev g of
          Just Bot -> Node (ctx, trm, Just typ1) [g]
          _ -> Node (ctx, trm, Nothing) [g]
