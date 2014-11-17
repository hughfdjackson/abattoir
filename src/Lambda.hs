module Lambda (
  Lambda(..),
  eval
) where

data Lambda = Var Name
            | Abstraction Name Lambda
            | Application Lambda Lambda
            deriving (Eq)
                     
instance Show Lambda where
  show (Var name)         = name
  show (Abstraction n l)  = "(λ" ++ n ++ "." ++ show l ++ ")"
  show (Application l l') = show l ++ " " ++ show l'

type Name = String

-- evaluation - i.e. bete reduction
eval :: Lambda -> Lambda
eval term@(Var _)  = term
eval term@(Abstraction _ _) = term
eval (Application (Abstraction n l) v) = substitute n l v

-- applyWithName :: Name -> Lambda -> Lambda -> Lambda
-- applyWithName name a@(Abstraction name' l) v = if name == name' then a
--                                                else substitute name l v

substitute name (Var name') v = if name' == name then v else Var name'
substitute name a@(Abstraction name' l) v  =  if name' == name then a else Abstraction name' (substitute name l v)
substitute name (Application l l') v       = Application (substitute name l v) (substitute name l' v)

-- (λx.(λy.x)) z
-- (λz.(λy.z))
-- (λy.z)
