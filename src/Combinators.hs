module Combinators(
  i,
  k,
  s,
  zero
) where

import Lambda

i :: Expr
i = L 'x' (V 'x')

k :: Expr
k = L 'x' (L 'y' (V 'x'))

s :: Expr
s = L 'w' (L 'y' (L 'x' (Ap (V 'y') (Ap (Ap (V 'w') (V 'y')) (V 'x')))))

zero :: Expr
zero = L 's' (L 'z' (V 'z'))