module Linear.Lang where

import Abt.Abt
import Abt.Operator
import Abt.Variable

data LinTp = Unit | Arr LinTp LinTp
  deriving Eq

data LinOp = Lam | Triv | App | Asc LinTp
  deriving Eq

instance Operator LinOp where
  arity Lam = [1]
  arity Triv = []
  arity App = [0, 0]
  arity (Asc t) = [0]

{--
typecheck :: (Abt t) => t LinOp -> VarState LinTp
typecheck t =
  case toAbtView t of
    AVar v -> 
--}
