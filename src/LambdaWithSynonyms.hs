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
import Data.Functor ((<$>))
import qualified Combinators
import Data.Map as Map


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

-- synonyms
type Synonyms = Map.Map Char Expr

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
defaultSynonyms = Map.fromList [('I', Combinators.i),
                                ('0', Combinators.zero),
                                ('S', Combinators.s)]

-- evaluation
eval' :: Synonyms -> Expr' -> Either String Expr
eval' syns expr = eval <$> substituteSynonyms syns expr

evalSteps' :: Synonyms -> Expr' -> Either String [String]
evalSteps' syns expr' = evalSteps <$> substituteSynonyms syns expr'
