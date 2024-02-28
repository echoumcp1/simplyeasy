module Checker where
import Data
import Control.Monad (unless)
import Eval

type Result a = Either String a

type Context = [(Name, Type)]

typeI0 :: Context -> ITerm -> Result Type
typeI0 = typeI 0

typeI :: Int -> Context -> ITerm -> Result Type
typeI i gamma (Ann cterm ty) =
    do typeC i gamma ty VStar
       let v = evalC ty []
       typeC i gamma cterm v
       return v
typeI i gamma Star = return VStar
typeI i gamma (Pi a b) =
    do typeC i gamma a VStar
       let v = evalC a []
       typeC (i + 1) ((Bound i, v) : gamma)
             (substC 0 (Par (Bound i)) b) VStar
       return VStar
typeI i gamma (Par x) =
    case lookup x gamma of
        Just v -> return v
        Nothing -> throwError "unknown identifier"
typeI i gamma (e1 :@: e2) =
    do sigma <- typeI i gamma e1
       case sigma of
           VPi v f -> do typeC i gamma e2 v
                         return (f (evalC e2 []))
           _ -> throwError "illegal application"
typeI i gamma Nat = return VStar
typeI i gamma (NatElim motive fZero fSucc res) =
    do typeC i gamma motive (VPi VNat (const VStar))
       let mVal = evalC motive []
       typeC i gamma fZero (mVal `vapp` VZero)
       typeC i gamma fSucc
        (VPi VNat (\n ->
         VPi (mVal `vapp` n) (\_ -> mVal `vapp` VSucc n)))
       typeC i gamma res VNat
       let nVal = evalC res []
       return (mVal `vapp` nVal)
typeI i gamma (Vec a n) =
    do typeC i gamma a VStar
       typeC i gamma n VNat
       return VStar
typeI i gamma (VecElim a motive fNil fCons sz vs) =
    do  typeC i gamma a VStar
        let aVal = evalC a []
        typeC i gamma motive (VPi VNat (\n -> VPi (VVec aVal n) (const VStar)))
        let mVal = evalC motive []
        typeC i gamma fNil (foldl vapp mVal [VZero,VNil aVal])
        typeC i gamma fCons
            (VPi VNat (\n ->
             VPi aVal (\y ->
             VPi (VVec aVal n) (\ys ->
             VPi (foldl vapp mVal [n,ys]) (\_ ->
             foldl vapp mVal [VSucc n, VCons aVal n y ys])))))
        typeC i gamma sz VNat
        let nVal = evalC sz []
        typeC i gamma vs (VVec aVal nVal)
        let vsVal = evalC vs []
        return (foldl vapp mVal [nVal, vsVal])

typeC :: Int -> Context -> CTerm -> Type -> Result ()
typeC i gamma (Inf e) v =
    do v' <- typeI i gamma e
       unless (quote0 v == quote0 v') (throwError "type mismatch")
typeC i gamma (Lam e) (VPi v f) =
    typeC (i + 1) ((Bound i, v):gamma) (substC 0 (Par (Bound i)) e) (f (vpar (Bound i)))
typeC i gamma Zero VNat = return ()
typeC i gamma (Succ k) VNat = typeC i gamma k VNat
typeC i gamma (Nil a) (VVec bVal VZero) =
    do typeC i gamma a VStar
       let aVal = evalC a []
       unless (quote0 aVal == quote0 bVal) (throwError "type mismatch")
typeC i gamma (Cons a sz x xs) (VVec bVal (VSucc k)) =
    do typeC i gamma a VStar
       let aVal = evalC a []
       unless (quote0 aVal == quote0 bVal) (throwError "type mismatch")
       typeC i gamma sz VNat
       let nVal = evalC sz []
       unless (quote0 nVal == quote0 k) (throwError "type mismatch")
       typeC i gamma x aVal
       typeC i gamma xs (VVec bVal k)
typeC i gamma _ _ = throwError "type mismatch"


substI :: Int -> ITerm -> ITerm -> ITerm
substI i r (Ann e t) = Ann (substC i r e) t
substI i r (Var j) = if i == j then r else Var j
substI i r (Par y) = Par y
substI i r (e1 :@: e2) = substI i r e1 :@: substC i r e2
substI i r Star = Star
substI i r (Pi a b) = Pi (substC i r a) (substC (i+1) r b)

substC :: Int -> ITerm -> CTerm -> CTerm
substC i r (Inf e) = Inf (substI i r e)
substC i r (Lam e) = Lam (substC (i + 1) r e)

quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Int -> Value -> CTerm
quote i (VLam f) = Lam (quote (i + 1) (f (vpar (Unquoted i))))
quote i (VNeutral n) = Inf (neutralQuote i n)
quote i VStar = Inf Star
quote i (VPi v f) = Inf (Pi (quote i v) (quote (i+1) (f (vpar (Unquoted i)))))

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote i (NPar x) = varpar i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

varpar :: Int -> Name -> ITerm
varpar i (Unquoted k) = Var (i - k - 1)
varpar i x = Par x

throwError :: String -> Result a
throwError = Left
