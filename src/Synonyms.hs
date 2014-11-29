module Synonyms (
  ExprWithSynonyms(..),
  substituteSynonyms
) where

import qualified Lambda as L
import Data.Map as Map

type Synonyms = Map Char L.Expr

data ExprWithSynonyms = V L.Name
                      | S L.Name
                      | L L.Name ExprWithSynonyms
                      | Ap ExprWithSynonyms ExprWithSynonyms
                      deriving (Eq, Show)

substituteSynonyms :: Synonyms -> ExprWithSynonyms -> Either String L.Expr
substituteSynonyms syns expr = case expr of
    (V name)        -> return $ L.V name
    (Ap expr expr') -> do
      lExpr  <- substituteSynonyms syns expr
      lExpr' <- substituteSynonyms syns expr'
      return $ L.Ap lExpr lExpr'
    (L name expr)   -> do
      lExpr <- substituteSynonyms syns expr
      return $ L.L name lExpr
    (S name)        -> handleLookupResult name $ Map.lookup name syns


handleLookupResult :: L.Name -> Maybe L.Expr -> Either String L.Expr
handleLookupResult key expr = case expr of
  (Just a)  -> Right a
  (Nothing) -> Left $ "Cannot find synonym " ++ [key]
