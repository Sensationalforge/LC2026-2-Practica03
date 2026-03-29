module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " y " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons a) = Cons a
fnn (Not(Not p)) = fnn p
fnn (Not (Var p)) = Not (Var p)
fnn (Not(Cons a)) = Not (Cons a)
fnn (Or p q) = (Or (fnn p) (fnn q))
fnn (And p q) = (And (fnn p) (fnn q))
fnn (Syss p q) = fnn (And(Impl p q) (Impl q p)) 
fnn (Impl p q) = fnn (Or(Not p) q)
fnn (Not (And p q)) = (Or (fnn(Not p)) (fnn(Not q)))
fnn (Not (Impl p q)) = (And (fnn p) (fnn(Not q)))
fnn (Not (Syss p q)) = fnn (Or (And p (Not q)) (And (Not p) q))
fnn (Not (Or p q)) = (And (fnn(Not p)) (fnn(Not q)))

--Ejercicio 2
fnc :: Prop -> Prop
fnc p = distribuir (fnn p)

distribuir :: Prop -> Prop
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir (Or p q) = distro (distribuir p) (distribuir q)
distribuir p = p

--Funcion auxiliar: Ayud a a distribuir mejor los casos de Or y And
distro :: Prop -> Prop -> Prop
distro (And p q) r = And (distro p r) (distro q r)
distro p (And q r) = And (distro p q) (distro p r)
distro p q = Or p q
{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [[Prop]]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas p = [dups (lister p)]

--Funcion auxiliar: Quita los ORS y pone cada cosa en la lista por separada
lister :: Prop -> [Prop]
lister (Or p q) = lister p ++ lister q
lister p = [p]

-- Funcion auxiliar: Checa si existen dos literales iguales en la lista
existencia :: Eq a => a -> [a] -> Bool
existencia _ [] = False
existencia x (y:ys) 
    | x == y    = True
    | otherwise = existencia x ys

-- Funcion auxiliar: Elimina los duplicados
dups :: Eq a => [a] -> [a]
dups [] = []
dups (x:xs)
    | existencia x xs = dups xs
    | otherwise   = x : dups xs

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 =
    if hayPar c1 c2
    then
        let (l1, l2) = encuentraPar c1 c2
            sin1 = eliminaPares l1 c1
            sin2 = eliminaPares l2 c2
        in dups (sin1 ++ sin2)
    else dups (c1 ++ c2)

--Funcion auxiliar: Checa si una literal en una clausula tiene su contraria en otra clausula
hayPar :: Clausula -> Clausula -> Bool
hayPar [] _ = False
hayPar (x:xs) ys =
    if complementar x ys
    then True
    else hayPar xs ys

--Funcion auxiliar: dice si dos literales son complementarias
pares :: Prop -> Prop -> Bool
pares (Not p) q = p == q
pares p (Not q) = p == q
pares _ _ = False

--Borra una literal contraria de cada clausula
eliminaPares :: Eq a => a -> [a] -> [a]
eliminaPares _ [] = []
eliminaPares x (y:ys)
    | x == y    = ys
    | otherwise = y : eliminaPares x ys

--Funcion auxiliar: Con un literal y una clausula, da un literal contrario al dado en la clausula
pares2 :: Literal -> Clausula -> Literal
pares2 _ [] = error "No hay"     --Se tiene que asumir que siempre regresan literal, pero haskell me mandaba error si no ponia esto :c
pares2 x (y:ys)
    | pares x y = y
    | otherwise = pares2 x ys

--Funcion auxiliar: Dice si una literal tiene complemento dentro de la misma clausula
complementar :: Literal -> Clausula -> Bool
complementar _ [] = False
complementar x (y:ys)
    | pares x y = True
    | otherwise          = complementar x ys

--Funcion auxiliar:Encuentra un par de literales opuestas en clausulas distintas
encuentraPar :: Clausula -> Clausula -> (Literal, Literal)
encuentraPar (x:xs) ys
    | complementar x ys = (x, pares2 x ys)
    | otherwise = encuentraPar xs ys
{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
--Ya había creado hayPar por accidente en el ejercicio anterior 
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = hayPar

--Ejercicio 2 --Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula. 
saturacion :: Prop -> Bool
saturacion p = evalClausulas (procedimiento (clausulas (fnc p)))

evalClausulas :: [Clausula] -> Bool
evalClausulas []       = True        
evalClausulas ([]:_)   = False       
evalClausulas (_:cs)   = evalClausulas cs

--Borra duplicados
dupsnew :: Eq a => [a] -> [a]
dupsnew [] = [] 
dupsnew (x:xs) = x : dupsnew [y | y <- xs, y /= x]

--Genera pares de clausulas
otrosp :: [Clausula] -> [(Clausula,Clausula)]
otrosp [] = [] 
otrosp (x:xs) = [(x,y) | y <- xs] ++ otrosp xs 

--Da resolvente entre dos clausulas
resolventes :: [Clausula] -> [Clausula]
resolventes claus = [ r | claus1 <- claus, claus2 <- claus, claus1 /= claus2, r <- resuelveLimpio claus1 claus2 ] 

--Crea la lista de todos los resolventes
reiterar :: [Clausula] -> [Clausula]
reiterar claus = claus ++ resolventes claus

-- Elimina literales y combina la lista, despues quita duplicados
resuelveLimpio claus1 claus2 =
    if hayPar claus1 claus2
    then
        let (lit1, lit2) = encuentraPar claus1 claus2
            sin1 = eliminaPares lit1 claus1
            sin2 = eliminaPares lit2 claus2
        in [dups (sin1 ++ sin2)]
    else []

-- Si hay cláusula vacía, devuelve una lista con eso, sino genera resolventes
procedimiento :: [Clausula] -> [Clausula]
procedimiento claus
    | vacia claus = [[]]
    | otherwise =
        let proxima = reiterar claus
            limpiaActual = dupsnew claus
            limpiaProxima = dupsnew proxima
        in if length limpiaActual == length limpiaProxima
           then limpiaActual
           else procedimiento limpiaProxima 

--Checa si hay clausula vacia en la lista
vacia :: [Clausula] -> Bool
vacia [] = False 
vacia ([]:_) = True 
vacia (_:cs) = vacia cs
