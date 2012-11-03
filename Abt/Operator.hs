module Abt.Operator where

class (Eq o) => Operator o where
  arity :: o -> [Int]
--  toString :: o -> String
