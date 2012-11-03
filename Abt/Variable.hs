module Abt.Variable where

import Control.Monad.State

newtype Var = Var { getVar :: Int }
  deriving Eq

type VarState a = State Var a

fresh :: VarState Var
fresh = do
  x <- get
  () <- put $ Var $ (getVar x) + 1
  return x
