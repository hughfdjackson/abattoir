module Lambda (
  Expr(..),
  Name,
  eval,
  isReducedForm,
  v, l, ap,
  combI, combK,
  showExpr,
  isInfinitelyRecursive
) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup, fromList)

type Name = Char

data Expr = V Name
          | L Name Expr Env 
          | Ap Expr Expr
          deriving (Eq, Show)

type Env = Map Name Expr
emptyEnv = empty
isEmptyEnv e = e == emptyEnv
env list = fromList list


v name = V name
l name expr = L name expr emptyEnv
ap expr expr' = Ap expr expr'

showExpr :: Expr -> String
showExpr expr = case expr of
  (V name)        -> toString name
  (Ap expr expr') -> showExpr expr ++ showExpr expr'
  (L name expr env)   -> "(λ" ++ toString name ++ "." ++ showExpr expr ++ ")"
  where toString n  = [n]
        showEnv env = if isEmptyEnv env then ""
                      else "env=" ++ show env

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
