module Lambda (
  Expr(..),
  Name,
  eval,
  isReducedForm,
  v, l, ap, env,
  combI, combK,
  showExpr,
  isInfinitelyRecursive
) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup, fromList, toList)
import Data.List (intercalate)
import Data.Functor ((<$>))

type Name = Char

data Expr = V Name
          | L Name Expr Env 
          | Ap Expr Expr
          deriving (Eq, Show)

v name = V name
l name expr = L name expr emptyEnv
ap expr expr' = Ap expr expr'

showExpr :: Expr -> String
showExpr expr = case expr of
  (V name)        -> showName name
  (Ap expr expr') -> showExpr expr ++ showExpr expr'
  (L name expr env)   -> "(λ" ++ showName name ++ "." ++ showExpr expr ++ showEnv env ++ ")"


showName :: Char -> String
showName = (: [])

-- Environment 
type Env = Map Name Expr

emptyEnv :: Env
emptyEnv = empty

isEmptyEnv :: Env -> Bool
isEmptyEnv e = e == emptyEnv

env :: [(Name,Expr)] -> Env
env list = fromList list

showEnv :: Env -> String
showEnv env
  | isEmptyEnv env = ""
  | otherwise      = "{" ++ (intercalate "," (formatPairs <$> (toList env))) ++ "}"
  where formatPairs (name,expr) = showName name ++ "=" ++ showExpr expr

eval :: Expr -> Expr
eval = evalWithEnv emptyEnv

evalWithEnv :: Env -> Expr -> Expr
evalWithEnv env (Ap expr expr')  = let (L name body env') = evalWithEnv env expr
                                       a                  = evalWithEnv env expr'
                                       closureEnv         = insert name a env'
                                   in evalWithEnv closureEnv body
evalWithEnv env term@(V name) = lookup name env `getOrElse` term
evalWithEnv env term@(L name body env') = L name body env
  
isReducedForm :: Expr -> Bool
isReducedForm (Ap _ _) = False
isReducedForm _        = True

getOrElse :: Maybe a -> a -> a
(Just a) `getOrElse` b = a
Nothing `getOrElse`  b = b

isInfinitelyRecursive :: Expr -> Bool
isInfinitelyRecursive expr = undefined
  
-- combinators
combK = (l 'x' (l 'y' (v 'x')))
combI = (l 'x' (v 'x'))
