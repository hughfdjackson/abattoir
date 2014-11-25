module Lambda (
  Expr(..),
  Name,
  substitute,
  eval,
  isReducedForm,
  v, l, ap,
  combI, combK,
  showExpr,
  eval,
  renameBoundTo,
  renameBoundWithout,
  freeNames,
  names,
  boundNames
) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup, fromList, toList)
import Data.List (intercalate, elemIndex)
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import Data.Either (Either(..))
import qualified Data.Set as Set

type Name = Char

data Expr = V Name
          | L Name Expr
          | Ap Expr Expr
          deriving (Eq, Show)

v name = V name
l name expr = L name expr
ap expr expr' = Ap expr expr'

showExpr :: Expr -> String
showExpr expr = case expr of
  (V name)        -> showName name
  (Ap expr expr') -> showExpr expr ++ showExpr expr'
  (L name expr)   -> "(λ" ++ showName name ++ "." ++ showExpr expr ++ ")"

showName :: Name -> String 
showName n = [n]

isReducedForm :: Expr -> Bool
isReducedForm (Ap _ _) = False
isReducedForm _        = True

-- combinators
combK = (l 'x' (l 'y' (v 'x')))
combI = (l 'x' (v 'x'))

eval :: Expr -> Either String Expr
eval (Ap inner@(Ap _ _) expr)      = do
  evalledInner <- eval inner
  eval (Ap evalledInner expr)
eval (Ap inner@(L name body) expr) = return $ substitute name expr body
eval (Ap v@(V _) expr)             = Left $ "cannot apply " ++ show expr ++ " to variable (" ++ show v ++ ")"
eval x = return x

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

newName :: Name -> Expr -> Name
newName = undefined

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
        available   = (Set.fromList ['a'..'z']) Set.\\ unavailable
        next        = head $ cycle $ Set.toList available
  
renameBoundTo :: Name -> Name -> Expr -> Expr
renameBoundTo name name' expr = renameBoundTo' expr
  where renameBoundTo' l@(L name'' expr) = if name'' == name then renameAllVariablesTo name name' l
                                         else L name'' (renameBoundTo' expr)
        renameBoundTo' (Ap expr expr') = Ap (renameBoundTo' expr) (renameBoundTo' expr')
        renameBoundTo' v = v
                                                

renameAllVariablesTo :: Name -> Name -> Expr -> Expr
renameAllVariablesTo name name' = mapNames (\n -> if n == name then name' else n)

mapNames :: (Name -> Name) -> Expr -> Expr
mapNames fn (L name expr)   = L (fn name) (mapNames fn expr)
mapNames fn (Ap expr expr') = Ap (mapNames fn expr) (mapNames fn expr')
mapNames fn (V name)        = V (fn name)

nextNameForExpr :: Name -> Expr -> Name
nextNameForExpr name expr = newName
  where banList   = Set.insert name $ names expr
        nameCycle = cycle $ Set.toList $ Set.difference (Set.fromList ['a'..'z']) banList
        oldNameIndex = elemIndex name nameCycle 
        newName = head $ drop (fromMaybe 0 oldNameIndex) nameCycle


