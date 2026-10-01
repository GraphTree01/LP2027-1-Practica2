type ID = String

data EAB = Num Int | Var ID | Bool Bool
            | Suma EAB EAB | Prod EAB EAB
            | Suc EAB | Pred EAB
            | Not EAB
            | If EAB EAB EAB
            | IsZero EAB
            | Lt EAB EAB | Gt EAB EAB | Eq EAB EAB
            | Let ID EAB EAB
            deriving (Eq)

type Env = [(ID, EAB)]

x, y, z :: ID
x = "x"
y = "y"
z = "z"

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
evalEnv env@((x, e):xs) (Var y) =
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
evalEnv env (Lt e1 e2) =
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
    if x == y then Let y (sust x e1 e2) e3
    else Let y (sust x e1 e2) (sust x e1 e3)
