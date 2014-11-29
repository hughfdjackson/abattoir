module LambdaWithSynonyms(
  Synonyms,
  Expr'(..),
  Name,
  substituteSynonyms,
  emptySynonyms,
  defaultSynonyms,
  eval',
  evalSteps'
) where

import Lambda (Expr(..), Name, eval, evalSteps)
import Data.Map as Map

type Synonyms = Map.Map Char Expr

data Expr' = V' Name
           | S' Name
           | L' Name Expr'
           | Ap' Expr' Expr'
          deriving (Eq)

instance Show Expr' where
  show expr = case expr of
      (S' name)                -> showName name
      (V' name)                -> showName name
      (Ap' expr' ap@(Ap' _ _)) -> show expr' ++ "(" ++ show ap ++ ")"
      (Ap' expr' arg)          -> show expr' ++ show arg
      l@(L' _ _)               -> "(λ" ++ showArgAndBody l ++ ")"
    where showArgAndBody (L' name body) = showName name ++ showArgAndBody body
          showArgAndBody (V' name)      =  "." ++ showName name
          showArgAndBody (S' name)      =  "." ++ showName name
          showArgAndBody ap@(Ap' _ _)   = "." ++ show ap

showName :: a -> [a]
showName n = [n]

substituteSynonyms :: Synonyms -> Expr' -> Either String Expr
substituteSynonyms syns expr = case expr of
    (V' name)        -> return $ V name
    (Ap' e e')       -> do
      lExpr  <- substituteSynonyms syns e
      lExpr' <- substituteSynonyms syns e'
      return $ Ap lExpr lExpr'
    (L' name e)      -> do
      lExpr <- substituteSynonyms syns e
      return $ L name lExpr
    (S' name)        -> handleLookupResult name $ Map.lookup name syns

handleLookupResult :: Name -> Maybe Expr -> Either String Expr
handleLookupResult key expr = case expr of
  (Just a)  -> Right a
  (Nothing) -> Left $ "Cannot find synonym " ++ [key]

emptySynonyms :: Synonyms
emptySynonyms = Map.empty

defaultSynonyms :: Synonyms
defaultSynonyms = emptySynonyms

-- evaluation
eval' :: Synonyms -> Expr' -> Either String Expr
eval' syns expr = substituteSynonyms syns expr >>= eval

evalSteps' :: Synonyms -> Expr' -> Either String [String]
evalSteps' syns expr' = substituteSynonyms syns expr' >>= evalSteps
