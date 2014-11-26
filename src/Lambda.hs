module Lambda (
  Expr(..),
  Name,
  eval,
  substitute,
  combI, combK,
  renameBoundTo,
  renameBoundWithout,
  freeNames,
  names,
  boundNames
) where

import           Data.Either
import           Data.List
import           Data.Map        (Map, empty, fromList, insert, lookup, toList)
import           Data.Maybe
import qualified Data.Set        as Set
import           Prelude         hiding (lookup)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements)

type Name = Char

data Expr = V Name
          | L Name Expr
          | Ap Expr Expr
          deriving (Eq)

instance Show Expr where
  show expr = case expr of
    (V name)                -> showName name
    (Ap expr' ap@(Ap _ _))  -> show expr' ++ "(" ++ show ap ++ ")"
    (Ap expr' arg)          -> show expr' ++ show arg
    (L name body)           -> "(λ" ++ showName name ++ "." ++ show body ++ ")"
    where showName n = [n]


instance Arbitrary Expr where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    case n of
      0 -> do name <- possibleNames
              return $ V name

      1 -> do name <- possibleNames
              expr <- arbitrary
              return $ L name expr

      2 -> do expr  <- arbitrary
              expr' <- arbitrary
              name  <- possibleNames
              return $ Ap (L name expr) expr'
    where possibleNames = elements ['a'..'z']



-- Combinators
combK :: Expr
combK = L 'x' (L 'y' (V 'x'))

combI :: Expr
combI = L 'x' (V 'x')


-- Evaluation
eval :: Expr -> Either String Expr
eval expr = case expr of
  (Ap v@(V _) arg)              -> Left $ "cannot apply " ++ show arg ++ " to variable (" ++ show v ++ ")"
  l@(L _ _)                     -> return l
  v@(V _)                       -> return v
  (Ap (L name body) arg)        -> return $ substitute name arg body
  (Ap inner@(Ap _ _) arg)      -> do
    evalledInner <- eval inner
    eval (Ap evalledInner arg)


substitute :: Name -> Expr -> Expr -> Expr
substitute name arg expr = subIn cleanedExpr
  where subIn = straightforwardSubstitute name arg
        blacklist = freeNames arg
        cleanedExpr = foldl (flip (renameBoundWithout blacklist)) expr (Set.toList blacklist)


straightforwardSubstitute :: Name -> Expr -> Expr -> Expr
straightforwardSubstitute n e l@(L x expr)    = if x /= n then L x (straightforwardSubstitute n e expr) else l
straightforwardSubstitute n e (Ap expr expr') = Ap (straightforwardSubstitute n e expr) (straightforwardSubstitute n e expr')
straightforwardSubstitute n e v@(V name)      = if name == n then e else v

boundNames :: Expr -> Set.Set Name
boundNames expr = boundNames' expr Set.empty
  where boundNames' (L name expr)   names = Set.insert name names
        boundNames' (Ap expr expr') names = Set.union (boundNames' expr names) (boundNames' expr' names)
        boundNames' (V name)        names = names


freeNames :: Expr -> Set.Set Name
freeNames expr = freeNames' expr Set.empty
  where freeNames' (L name expr) names   = freeNames' expr (Set.insert name names)
        freeNames' (Ap expr expr') names = Set.union (freeNames' expr names) (freeNames' expr' names)
        freeNames' (V name) names        = if Set.member name names then Set.empty
                                           else Set.singleton name

names :: Expr -> Set.Set Name
names expr = names' expr Set.empty
  where names' (L n expr) names      = Set.union (Set.insert n names) (names' expr names)
        names' (Ap expr expr') names = Set.union (names' expr names) (names' expr' names)
        names' (V n)           names = Set.singleton n

renameBoundWithout :: Set.Set Name -> Name -> Expr -> Expr
renameBoundWithout blacklist name expr = renameBoundTo name next expr
  where unavailable = Set.union (names expr) blacklist
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

