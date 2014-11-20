module Lambda (
  Expr(..),
  Name,
  eval,
  isReducedForm,
  v, l, ap
) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup)

type Name = Char

data Expr = V Name
          | L Name Expr Env 
          | Ap Expr Expr
          deriving (Eq, Show)

type Env = Map Name Expr

emptyEnv = empty

v name = V name
l name expr = L name expr emptyEnv
ap expr expr' = Ap expr expr'

-- instance Show Expr where
--   show (V name)        = toString name
--   show (Ap expr expr') = show expr ++ show expr'
--   show (L name expr)   = "(λ" ++ toString name ++ "." ++ show expr ++ ")"
-- toString :: Char -> String
-- toString n = [n]

-- evaluation
eval :: Expr -> Expr
eval = evalWithEnv emptyEnv

-- (ap (lx.(ly.x)) t) = [x/z] = (ly.x {env: x=z})

-- (ap (ap (lx.(ly.x)) a) b)
--   = [x/a] = (ap (ly.x {env: x=a}) b)
--   = [b/y] = (x {env: x=a,y=b})
--   = a

-- (ap (lx.x) (ap (lx.x) (lx.x)))
-- (ap (lx.x) (lx.x))
-- (lx.x)

-- (lx.x)
-- x
evalStep :: Env -> Expr -> Expr
evalStep env l@(L _ _ _) = l
evalStep env l@(L _ _ _) = l

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
  
