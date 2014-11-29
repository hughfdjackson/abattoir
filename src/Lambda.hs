module Lambda (
  Expr(..),
  Name,
  Synonyms,
  substituteSynonyms,
  synonymsEmpty,
  eval,
  evalSteps,
  substitute,
  combI, combK,
  renameBoundTo,
  renameBoundWithout,
  freeNames,
  names,
  boundNames
) where

import           Control.Monad.Trans
import           Control.Monad.Trans.Writer.Lazy
import           Data.Set as Set
import qualified Data.Map as Map
import           Test.QuickCheck                 (Arbitrary, Gen, arbitrary,
                                                  choose, elements)

type Synonyms = Map.Map Char Expr

type Name = Char
data Expr = V Name
          | S Name
          | L Name Expr
          | Ap Expr Expr
          deriving (Eq)

instance Show Expr where
  show expr = case expr of
      (V name)                -> showName name
      (S name)                -> showName name
      (Ap expr' ap@(Ap _ _))  -> show expr' ++ "(" ++ show ap ++ ")"
      (Ap expr' arg)          -> show expr' ++ show arg
      l@(L _ _)               -> "(λ" ++ showArgAndBody l ++ ")"

showArgAndBody :: Expr -> String
showArgAndBody expr = case expr of
  (L name body) -> showName name ++ showArgAndBody body
  (V name)      -> "." ++ showName name
  (S name)      -> "." ++ showName name
  ap@(Ap _ _)   -> "." ++ show ap

showName :: a -> [a]
showName n = [n]

instance Arbitrary Expr where
  arbitrary = do
    n <- choose (0, 3) :: Gen Int
    case n of
      0 -> do name <- possibleNames
              return $ V name

      1 -> do name <- possibleSynonyms
              return $ S name

      2 -> do name <- possibleNames
              expr <- arbitrary
              return $ L name expr

      _ -> do expr  <- arbitrary
              expr' <- arbitrary
              name  <- possibleNames
              return $ Ap (L name expr) expr'
    where possibleNames = elements ['a'..'z']
          possibleSynonyms = elements $ ['A'..'Z'] ++ ['0'..'9']


substituteSynonyms :: Synonyms -> Expr -> Either String Expr
substituteSynonyms syns expr = case expr of
    (V name)        -> return $ V name
    (Ap e e')       -> do
      lExpr  <- substituteSynonyms syns e
      lExpr' <- substituteSynonyms syns e'
      return $ Ap lExpr lExpr'
    (L name e)      -> do
      lExpr <- substituteSynonyms syns e
      return $ L name lExpr
    (S name)        -> handleLookupResult name $ Map.lookup name syns

synonymsEmpty :: Synonyms
synonymsEmpty = Map.empty

handleLookupResult :: Name -> Maybe Expr -> Either String Expr
handleLookupResult key expr = case expr of
  (Just a)  -> Right a
  (Nothing) -> Left $ "Cannot find synonym " ++ [key]


-- Combinators
combK :: Expr
combK = L 'x' (L 'y' (V 'x'))

combI :: Expr
combI = L 'x' (V 'x')

-- Evaluation
data Entry = Substitute Expr Name Expr Expr Expr

instance Show Entry where
  show (Substitute arg name body original result) =
       show original ++ " == " ++ "[" ++ show arg ++ "/" ++ showName name ++ "] "
    ++ show body ++ " == " ++ show result


eval :: Expr -> Either String Expr
eval = fmap fst . runWriterT . evalWithSteps

evalSteps :: Expr -> Either String [String]
evalSteps = fmap (fmap show) . execWriterT . evalWithSteps

-- evals an expression, producing the result and a step-by-step list of
-- the actions that went into it
evalWithSteps :: Expr -> WriterT [Entry] (Either String) Expr
evalWithSteps expr = case expr of
    (S name)                         -> synonymError name
    (Ap (S name) arg)                -> synonymError name
    (Ap v@(V _) arg)                 -> lift $ Left $ "cannot apply " ++ show arg ++ " to variable (" ++ show v ++ ")"
    l@(L _ _)                        -> return l
    v@(V _)                          -> return v
    original@(Ap (L name body) arg)  -> do
      let result = substitute name arg body
      tell [Substitute arg name body original result]
      evalWithSteps result
    (Ap inner@(Ap _ _) arg)      -> do
      evalledInner <- evalWithSteps inner
      evalWithSteps (Ap evalledInner arg)
  where synonymError name = lift $ Left $  "cannot evaluate a synonym that hasn't been substituted for its lambda expression: " ++ showName name

substitute :: Name -> Expr -> Expr -> Expr
substitute name arg expr = subIn cleanedExpr
  where subIn = straightforwardSubstitute name arg
        blacklist = freeNames arg
        cleanedExpr = Prelude.foldl (flip (renameBoundWithout blacklist)) expr (toList blacklist)


straightforwardSubstitute :: Name -> Expr -> Expr -> Expr
straightforwardSubstitute n e l@(L x expr)    = if x /= n then L x (straightforwardSubstitute n e expr) else l
straightforwardSubstitute n e (Ap expr expr') = Ap (straightforwardSubstitute n e expr) (straightforwardSubstitute n e expr')
straightforwardSubstitute n e v@(V name)      = if name == n then e else v

boundNames :: Expr -> Set Name
boundNames expr = boundNames' expr empty
  where boundNames' (L name _) ns = insert name ns
        boundNames' (Ap e e')  ns = boundNames' e ns `union` boundNames' e' ns
        boundNames' (V _)      ns = ns


freeNames :: Expr -> Set Name
freeNames expr = freeNames' expr empty
  where freeNames' (L name e) ns = freeNames' e (insert name ns)
        freeNames' (Ap e e')  ns = freeNames' e ns `union` freeNames' e' ns
        freeNames' (V name)   ns = if member name ns then empty
                                   else Set.singleton name

names :: Expr -> Set Name
names expr = names' expr empty
  where names' (L n e)   ns = insert n ns `union` names' e ns
        names' (Ap e e') ns = names' e ns `union` names' e' ns
        names' (V n)     _  = Set.singleton n

renameBoundWithout :: Set Name -> Name -> Expr -> Expr
renameBoundWithout blacklist name expr = renameBoundTo name next expr
  where unavailable = names expr `union` blacklist
        available   = Set.fromList ['a'..'z'] Set.\\ unavailable
        next        = head $ cycle $ Set.toList available

renameBoundTo :: Name -> Name -> Expr -> Expr
renameBoundTo name name' = renameBoundTo'
  where renameBoundTo' l@(L name'' expr) = if name'' == name then renameAllTo name name' l
                                         else L name'' (renameBoundTo' expr)
        renameBoundTo' (Ap expr expr') = Ap (renameBoundTo' expr) (renameBoundTo' expr')
        renameBoundTo' v = v

renameAllTo :: Name -> Name -> Expr -> Expr
renameAllTo name name' = mapNames (\n -> if n == name then name' else n)

mapNames :: (Name -> Name) -> Expr -> Expr
mapNames fn (L name expr)   = L (fn name) (mapNames fn expr)
mapNames fn (Ap expr expr') = Ap (mapNames fn expr) (mapNames fn expr')
mapNames fn (V name)        = V (fn name)

