module Data where

data Name
    = Const String
    | Bound Int
    | Unquoted Int
    deriving (Show, Eq)

type Type = Value

data ITerm 
    = Ann CTerm CTerm
    | Star
    | Pi CTerm CTerm
    | Var Int
    | Par Name
    | ITerm :@: CTerm
    | Nat
    | NatElim CTerm CTerm CTerm CTerm
    | Vec CTerm CTerm
    | VecElim CTerm CTerm CTerm CTerm CTerm CTerm
    deriving (Show, Eq)

data CTerm
    = Inf ITerm
    | Lam CTerm
    | Zero 
    | Succ CTerm
    | Nil CTerm
    | Cons CTerm CTerm CTerm CTerm
    deriving (Show, Eq)

data Value
    = VLam (Value -> Value)
    | VStar
    | VPi Value (Value -> Value)
    | VNeutral Neutral
    | VNat
    | VZero
    | VSucc Value 
    | VNil Value
    | VCons Value Value Value Value
    | VVec Value Value

data Neutral
    = NPar Name
    | NApp Neutral Value
    | NNatElim Value Value Value Neutral
    | NVecElim Value Value Value Value Value Neutral

type Env = [Value]