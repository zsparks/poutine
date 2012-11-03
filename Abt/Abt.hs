module Abt.Abt where

import Abt.Variable
import Abt.Operator
import Control.Monad
import Data.Functor

data AbtView o a = AVar Var | AApp o [a] | AAbs Var a

class Abt t where
  toAbtView :: t o -> VarState (AbtView o (t o))
  fromAbtView :: AbtView o (t o) -> t o

data DeBruijnAbt o =
  FVAR Var | BVAR Int | APP o [DeBruijnAbt o] | ABS (DeBruijnAbt o)
  deriving Eq

unbind :: Int -> Var -> DeBruijnAbt o -> DeBruijnAbt o
unbind i x (FVAR v) = FVAR v
unbind i x (BVAR j) = if i == j then FVAR x else BVAR j
unbind i x (APP o es) = APP o $ unbind i x <$> es
unbind i x (ABS e) = ABS $ unbind (i + 1) x e

bind :: Int -> Var -> DeBruijnAbt o -> DeBruijnAbt o
bind i x (FVAR v) = if x == v then BVAR i else FVAR v
bind i x (BVAR j) = BVAR j
bind i x (APP o es) = APP o $ bind i x <$> es
bind i x (ABS e) = ABS $ bind (i + 1) x e

toView :: DeBruijnAbt o -> VarState (AbtView o (DeBruijnAbt o))
toView (FVAR v) = return $ AVar v
toView (BVAR i) = undefined
toView (APP o es) = return $ AApp o es
toView (ABS e) = do
  x <- fresh
  return $ AAbs x $ unbind 0 x e

fromView :: AbtView o (DeBruijnAbt o) -> DeBruijnAbt o
fromView (AVar v) = FVAR v
fromView (AApp o es) = APP o es
fromView (AAbs v e) = ABS $ bind 0 v e

instance Abt DeBruijnAbt where
  toAbtView = toView
  fromAbtView = fromView
