module Lambda (
  Expr(..),
  Name,
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
import           Test.QuickCheck                 (Arbitrary, Gen, arbitrary,
                                                  choose, elements)

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

showName :: a -> [a]
showName n = [n]


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
data Entry = Substitute Expr Name Expr Expr

instance Show Entry where
  show (Substitute arg name original result) = "[" ++ show arg ++ "/" ++ showName name ++ "] "
                                            ++ show original ++ " == " ++ show result


eval :: Expr -> Either String Expr
eval = fmap fst . runWriterT . evalWithSteps

evalSteps :: Expr -> Either String [String]
evalSteps = fmap (fmap show) . execWriterT . evalWithSteps

-- evals an expression, producing the result and a step-by-step list of
-- the actions that went into it
evalWithSteps :: Expr -> WriterT [Entry] (Either String) Expr
evalWithSteps expr = case expr of
               (Ap v@(V _) arg)                 -> lift $ Left $ "cannot apply " ++ show arg ++ " to variable (" ++ show v ++ ")"
               l@(L _ _)                        -> return l
               v@(V _)                          -> return v
               (Ap original@(L name body) arg)  -> do
                  let result = substitute name arg body
                  tell [Substitute arg name original result]
                  evalWithSteps result

               (Ap inner@(Ap _ _) arg)      -> do
                 evalledInner <- evalWithSteps inner
                 evalWithSteps (Ap evalledInner arg)


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
  where boundNames' (L name expr)   names = insert name names
        boundNames' (Ap expr expr') names = boundNames' expr names `union` boundNames' expr' names
        boundNames' (V name)        names = names


freeNames :: Expr -> Set Name
freeNames expr = freeNames' expr empty
  where freeNames' (L name expr) names   = freeNames' expr (insert name names)
        freeNames' (Ap expr expr') names = freeNames' expr names `union` freeNames' expr' names
        freeNames' (V name) names        = if member name names then empty
                                           else Set.singleton name

names :: Expr -> Set Name
names expr = names' expr empty
  where names' (L n expr) names      = insert n names `union` names' expr names
        names' (Ap expr expr') names = names' expr names `union` names' expr' names
        names' (V n)           names = Set.singleton n

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

