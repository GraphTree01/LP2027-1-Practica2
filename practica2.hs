import Data.Tuple.Experimental (CTuple0, CTuple3)
import Foreign.C (e2BIG)

type ID = String

data EAB
  = Num Int
  | Var ID
  | Bool Bool
  | Suma EAB EAB
  | Prod EAB EAB
  | Suc EAB
  | Pred EAB
  | Not EAB
  | If EAB EAB EAB
  | IsZero EAB
  | Lt EAB EAB
  | Gt EAB EAB
  | Eq EAB EAB
  | Let ID EAB EAB
  deriving (Eq)

type Env = [(ID, EAB)]

x, y, z :: ID
x = "x"
y = "y"
z = "z"

type Ctx = [(ID, Type)]

data Type = Nat | Boolean
  deriving (Eq, Show)

---------------------------------------------------------------

instance Show EAB where
  show (Num n) = show n
  show (Var x) = x
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Suma e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Prod e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
  show (Suc e) = "suc(" ++ show e ++ ")"
  show (Pred e) = "pred(" ++ show e ++ ")"
  show (Not e) = "not (" ++ show e ++ ")"
  show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (IsZero e) = "isZero(" ++ show e ++ ")"
  show (Lt e1 e2) = "(" ++ show e1 ++ "<" ++ show e2 ++ ")"
  show (Gt e1 e2) = "(" ++ show e1 ++ ">" ++ show e2 ++ ")"
  show (Eq e1 e2) = "(" ++ show e1 ++ "==" ++ show e2 ++ ")"
  show (Let x e1 e2) = "(Let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

evalEnv :: Env -> EAB -> Either Int Bool
evalEnv _ (Num n) = Left n
evalEnv _ (Bool b) = Right b
evalEnv [] (Var x) = error ("La variable " ++ x ++ " no esta en el ambiente")
evalEnv env@((x, e) : xs) (Var y) =
  if x == y then evalEnv env e else evalEnv xs (Var y)
evalEnv env (Suma e1 e2) =
  case (evalEnv env e1, evalEnv env e2) of
    (Left n1, Left n2) -> Left (n1 + n2)
evalEnv env (Prod e1 e2) =
  case (evalEnv env e1, evalEnv env e2) of
    (Left n1, Left n2) -> Left (n1 * n2)
evalEnv env (Suc e) =
  case (evalEnv env e) of
    (Left n) -> Left (n + 1)
evalEnv env (Pred e) =
  case (evalEnv env e) of
    (Left n) -> Left (n - 1)
evalEnv env (Not e) =
  case (evalEnv env e) of
    (Right b) -> if b == True then (Right False) else (Right True)
evalEnv env (If e e1 e2) =
  case (evalEnv env e, e1, e2) of
    (Right b, e1, e2) -> if b == True then evalEnv env e1 else evalEnv env e2
evalEnv env (IsZero e) =
  case (evalEnv env e) of
    (Right b) -> Right b
evalEnv env (Lt e1 e2) =
  case (evalEnv env e1, evalEnv env e2) of
    (Left n1, Left n2) -> Right (n1 < n2)
evalEnv env (Gt e1 e2) =
  case (evalEnv env e1, evalEnv env e2) of
    (Left n1, Left n2) -> Right (n1 > n2)
evalEnv env (Eq e1 e2) =
  case (evalEnv env e1, evalEnv env e2) of
    (Left n1, Left n2) -> Right (n1 == n2)
evalEnv env (Let x e1 e2) =
  case (evalEnv env e1, e2) of
    (Right b, e2) -> evalEnv (env ++ [(x, (Bool b))]) e2
    (Left n, e2) -> evalEnv (env ++ [(x, (Num n))]) e2

sust :: ID -> EAB -> EAB -> EAB
sust _ _ (Num n) = Num n
sust _ _ (Bool b) = Bool b
sust x e1 (Var y) = if x == y then e1 else Var y
sust x e1 (Suma e2 e3) = Suma (sust x e1 e2) (sust x e1 e3)
sust x e1 (Prod e2 e3) = Prod (sust x e1 e2) (sust x e1 e3)
sust x e1 (Suc e) = Suc (sust x e1 e)
sust x e1 (Pred e) = Pred (sust x e1 e)
sust x e1 (Not e) = Not (sust x e1 e)
sust x e1 (If e2 e3 e4) = If (sust x e1 e2) (sust x e1 e3) (sust x e1 e4)
sust x e1 (IsZero e) = IsZero (sust x e1 e)
sust x e1 (Lt e2 e3) = Lt (sust x e1 e2) (sust x e1 e3)
sust x e1 (Gt e2 e3) = Gt (sust x e1 e2) (sust x e1 e3)
sust x e1 (Eq e2 e3) = Eq (sust x e1 e2) (sust x e1 e3)
sust x e1 (Let y e2 e3) =
  if x == y
    then Let y (sust x e1 e2) e3
    else Let y (sust x e1 e2) (sust x e1 e3)

------------------PARTE 2 - SEMÁNTICA DINÁMICA -----
evalStep :: EAB -> EAB
evalStep (Num n) = Num n
evalStep (Bool b) = Bool b
evalStep (Var x) = Var x
evalStep (Suma (Num n) (Num m)) = Num (n + m)
evalStep (Suma (Num n) e2) = Suma (Num n) (evalStep e2)
evalStep (Suma e1 e2) = Suma (evalStep e1) e2
evalStep (Prod (Num n) (Num m)) = Num (n * m)
evalStep (Prod (Num n) e2) = Prod (Num n) (evalStep e2)
evalStep (Prod e1 e2) = Prod (evalStep e1) e2
evalStep (Pred (Num n)) = Num (n - 1)
evalStep (Pred e1) = Pred (evalStep e1)
evalStep (Suc (Num n)) = Num (n + 1)
evalStep (Suc e1) = Suc (evalStep e1)
evalStep (Not (Bool true)) = Bool False
evalStep (Not e1) = Not (evalStep e1)
evalStep (If (Bool True) e2 e3) = e2
evalStep (If (Bool False) e2 e3) = e3
evalStep (If e1 e2 e3) = If (evalStep e1) e2 e3
evalStep (IsZero (Num n)) = if (n /= 0) then Bool False else Bool True
evalStep (IsZero e1) = IsZero (evalStep e1)
evalStep (Let x (Num n) e3) = sust x (Num n) e3
evalStep (Let x (Bool b) e3) = sust x (Bool b) e3
evalStep (Let x e2 e3) = Let x (evalStep e2) e3
evalStep (Lt (Num n) (Num m)) = Bool (n < m)
evalStep (Lt (Num n) e2) = Lt (Num n) (evalStep e2)
evalStep (Lt e1 e2) = Lt (evalStep e1) e2
evalStep (Gt (Num n) (Num m)) = Bool (n > m)
evalStep (Gt (Num n) e2) = Gt (Num n) (evalStep e2)
evalStep (Gt e1 e2) = Gt (evalStep e1) e2
evalStep (Eq (Num n) (Num m)) = Bool (n == m)
evalStep (Eq (Num n) e2) = Eq (Num n) (evalStep e2)
evalStep (Eq e1 e2) = Eq (evalStep e1) e2

evalDin :: EAB -> EAB
evalDin e =
  let e' = evalStep e
   in if e' == e
        then e
        else evalDin e'

isValid :: EAB -> Bool
isValid e =
  let e' = evalDin e
   in case e' of
        Num _ -> True
        Bool _ -> True
        _ -> False

------------------PARTE 3 - SEMÁNTICA ESTÁTICA -----

-- Ejercicio 1
typeEAB :: Ctx -> EAB -> Type
typeEAB _ (Num n) = Nat
typeEAB _ (Bool b) = Boolean
typeEAB ctx (Var x) = buscaVariable ctx x
typeEAB ctx (Suma a1 a2)
  | typeEAB ctx a1 /= Nat = error ("Expected Nat: " ++ show a1)
  | typeEAB ctx a2 /= Nat = error ("Expected Nat: " ++ show a2)
  | otherwise = Nat
typeEAB ctx (Prod a1 a2)
  | typeEAB ctx a1 /= Nat = error ("Expected Nat: " ++ show a1)
  | typeEAB ctx a2 /= Nat = error ("Expected Nat: " ++ show a2)
  | otherwise = Nat
typeEAB ctx (Suc s) = if typeEAB ctx s == Nat then Nat else error ("Expected Nat: " ++ show s)
typeEAB ctx (Pred p) = if typeEAB ctx p == Nat then Nat else error ("Expected Nat: " ++ show p)
typeEAB ctx (Not n) = if typeEAB ctx n == Boolean then Boolean else error ("Expected Boolean: " ++ show n)
typeEAB ctx (If e1 e2 e3)
  | typeEAB ctx e1 /= Boolean = error ("Expected Boolean: " ++ show e1)
  | tipo2 /= tipo3 = error ("Types in e2 and e3 are different. " ++ "e2 is: " ++ show tipo2 ++ ", e3 is: " ++ show tipo3)
  | otherwise = tipo2
  where
    tipo2 = typeEAB ctx e2
    tipo3 = typeEAB ctx e3
typeEAB ctx (IsZero n) = if typeEAB ctx n == Nat then Boolean else error ("Expected Nat: " ++ show n)
typeEAB ctx (Lt a1 a2)
  | typeEAB ctx a1 /= Nat = error ("Expected Nat: " ++ show a1)
  | typeEAB ctx a2 /= Nat = error ("Expected Nat: " ++ show a2)
  | otherwise = Boolean
typeEAB ctx (Gt a1 a2)
  | typeEAB ctx a1 /= Nat = error ("Expected Nat: " ++ show a1)
  | typeEAB ctx a2 /= Nat = error ("Expected Nat: " ++ show a2)
  | otherwise = Boolean
typeEAB ctx (Eq a1 a2)
  | typeEAB ctx a1 /= Nat = error ("Expected Nat: " ++ show a1)
  | typeEAB ctx a2 /= Nat = error ("Expected Nat: " ++ show a2)
  | otherwise = Boolean
typeEAB ctx (Let x valor e) = typeEAB ((x, typeEAB ctx valor) : ctx) e

-- Función auxiliar Ejercicio 1
buscaVariable :: Ctx -> ID -> Type
buscaVariable [] _ = error "Variable no definida"
buscaVariable ((id, t) : ctx) x = if id == x then t else buscaVariable ctx x

-- Ejercicio 2
evalEst :: EAB -> Either Int Bool
evalEst e =
  case typeEAB [] e of
    Boolean -> evalEnv [] e
    Nat -> evalEnv [] e
