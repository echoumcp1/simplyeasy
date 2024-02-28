module Eval where
import Data

vpar :: Name -> Value
vpar n = VNeutral $ NPar n

evalI :: ITerm -> Env -> Value
evalI (Ann e _) env = evalC e env
evalI (Par x) env = vpar x
evalI (Var i) env = env !! i
evalI (e1 :@: e2) env = vapp (evalI e1 env) (evalC e2 env)
evalI Star env = VStar
evalI (Pi a b) env = VPi (evalC a env) (\x -> evalC b (x:env))
evalI Nat env = VNat
evalI (NatElim motive fZero fSucc res) env =
    let fZeroVal = evalC fZero env
        fSuccVal = evalC fSucc env
        helper nVal = 
            case nVal of
                VZero -> fZeroVal
                VSucc k -> fSuccVal `vapp` k `vapp` helper k
                VNeutral n -> VNeutral (NNatElim (evalC motive env) fZeroVal fSuccVal n)
                _ -> error "eval natElim internal err"
    in helper (evalC res env)

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral $ NApp n v

evalC :: CTerm -> Env -> Value 
evalC (Inf i) env = evalI i env
evalC (Lam e) env = VLam $ \x -> evalC e (x:env)
evalC Zero env = VZero
evalC (Succ k) env = VSucc (evalC k env)


