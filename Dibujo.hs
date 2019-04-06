module Dibujo where
-- definir el lenguaje

-- data Basica a = trian1 a b c | trian2 a | trianD a | rectan a

data Dibujo a = Vacio
              | Basica a
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              | Rot45 (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)

-- composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> (a -> a)
comp f 0 = f
comp f n = comp f (n-1) . f

-- comp :: ((a -> a) -> Int -> a -> a
-- (comp _ 0 a )= a
-- comp fun n a = comp fun (n-1) (fun a)

rotar45 :: Dibujo a -> Dibujo a
rotar45 d = Rot45 d

-- rotaciones de múltiplos de 90.
rot90 :: Dibujo a -> Dibujo a
rot90 d = Rotar d

r180 :: Dibujo a -> Dibujo a
r180 d = (comp rot90 2) d

r270 :: Dibujo a -> Dibujo a
r270 d = (comp rot90 3) d

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d1 d2 = Apilar 50 50 d1 d2

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d1 d2 = Juntar 50 50 d1 d2

-- Superpone una figura con otra
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) d1 d2 = Encimar d1 d2

-- dada una figura la repite en cuatro cuadrantes
cuarteto :: Dibujo a -> Dibujo a
cuarteto d = Apilar 50 50 (Juntar 50 50 d d) (Juntar 50 50 d d)

-- Una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = Encimar (Encimar d (Rotar d)) (Encimar (r180 d) (r270 d))

-- cuadrado con la misma figura rotada `i` por `90` para `i \in \{1..3\}`.
ciclar :: Dibujo a -> Dibujo a
ciclar d = Apilar 50 50 (Juntar 50 50 d (Rotar d)) (Juntar 50 50 (r180 d) (r270 d))

-- ver un a como una figura
pureDibe :: a -> Dibujo a
pureDibe x = Basica x

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib fun (Basica d1) = Basica (fun d1)
mapDib fun (Rotar d1) = Rotar (mapDib fun d1)
mapDib fun (Rot45 d1) = Rot45 (mapDib fun d1)
mapDib fun (Espejar d1) = Espejar (mapDib fun d1)
mapDib fun (Apilar i1 i2 d1 d2) = Apilar i1 i2 (mapDib fun d1) (mapDib fun d2)
mapDib fun (Juntar i1 i2 d1 d2) = Juntar i1 i2 (mapDib fun d1) (mapDib fun d2)
mapDib fun (Encimar d1 d2) = Encimar (mapDib fun d1) (mapDib fun d2)

cambiar :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
cambiar fun (Basica d1) = fun d1
cambiar fun (Rotar d1) = Rotar (cambiar fun d1)
cambiar fun (Rot45 d1) = Rot45 (cambiar fun d1)
cambiar fun (Espejar d1) = Espejar (cambiar fun d1)
cambiar fun (Apilar i1 i2 d1 d2) = Apilar i1 i2 (cambiar fun d1) (cambiar fun d2)
cambiar fun (Juntar i1 i2 d1 d2) = Juntar i1 i2 (cambiar fun d1) (cambiar fun d2)
cambiar fun (Encimar d1 d2) = Encimar (cambiar fun d1) (cambiar fun d2)

sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (b -> b -> b) ->
       Dibujo a -> b
sem bas rot esp rot45 api juntar encimar (Basica d) = bas d
sem bas rot esp rot45 api juntar encimar (Rotar d) = rot (sem bas rot esp rot45 api juntar encimar d)
sem bas rot esp rot45 api juntar encimar (Rot45 d) = esp (sem bas rot esp rot45 api juntar encimar d)
sem bas rot esp rot45 api juntar encimar (Espejar d) = rot45 (sem bas rot esp rot45 api juntar encimar d)
sem bas rot esp rot45 api juntar encimar (Apilar i1 i2 d1 d2) = api i1 i2 (sem bas rot esp rot45 api juntar encimar d1) (sem bas rot esp rot45 api juntar encimar d2)
sem bas rot esp rot45 api juntar encimar (Juntar i1 i2 d1 d2) = juntar i1 i2 (sem bas rot esp rot45 api juntar encimar  d1) (sem bas rot esp rot45 api juntar encimar d2)
sem bas rot esp rot45 api juntar encimar (Encimar d1 d2) = encimar (sem bas rot esp rot45 api juntar encimar d1) (sem bas rot esp rot45 api juntar encimar d2)

type Pred a = a -> Bool

-- dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por una figura vacía.
limpia :: Pred a -> Dibujo a -> Dibujo a
limpia f d = cambiar (\x -> if (f x) then Vacio else Basica x) d

-- alguna básica satisface el predicado
anyDib :: Pred a -> Dibujo a -> Bool
anyDib f d = or (map f (every d))

-- todas las básicas satisfacen el predicadonte
allDib :: Pred a -> Dibujo a -> Bool
allDib f d = and (map f (every d))

-- strBasica :: Dibujo a -> String
-- strBasica _ = "bas"

-- describe la figura. Ejemplos:
--   desc (Basica b) (const "b") = "b"
--   desc (Rotar fa) db = "rot (" ++ desc fa db ++ ")"
-- la descripción de cada constructor son sus tres primeros
-- símbolos en minúscula
desc :: (a -> String) -> Dibujo a -> String
desc f d = sem f
               (\x -> "rot (" ++ x ++ ")")
               (\x -> "esp (" ++ x ++ ")")
               (\x -> "r45 (" ++ x ++ ")")
               (\n1 n2 x y -> "api (" ++ x ++ ", " ++ y ++ ")")
               (\n1 n2 x y -> "jun (" ++ x ++ ", " ++ y ++ ")")
               (\x y -> "enc (" ++ x ++ ", " ++ y ++ ")")
               d

-- junta todas las figuras básicas de un dibujo
every :: Dibujo a -> [a]
every d = sem (\x -> [x])
              id
              id
              id
              (\n1 n2 x y -> x ++ y)
              (\n1 n2 x y -> x ++ y)
              (\x y -> x ++ y)
              d

-- cuenta la cantidad de veces que aparecen las básicas en una
-- figura.
contar :: Eq a => Dibujo a -> [(a,Int)]
contar d = map (\x -> (x, (length . filter( == x)) (every d))) (every d)

-- hay 4 rotaciones seguidas (empezando en el tope)
esRot360 :: Pred (Dibujo a)
esRot360 (Rotar(Rotar(Rotar(Rotar d)))) = True
esRot360 (Basica d) = False
esRot360 (Rotar d) = esRot360 d
esRot360 (Rot45 d) = esRot360 d
esRot360 (Espejar d) = esRot360 d
esRot360 (Apilar _ _ d1 d2) = (esRot360 d1) || (esRot360 d2)
esRot360 (Juntar _ _ d1 d2) = (esRot360 d1) || (esRot360 d2)
esRot360 (Encimar d1 d2) = (esRot360 d1) || (esRot360 d2)

-- hay 2 espejados seguidos (empezando en el tope)
esFlip2 :: Pred (Dibujo a)
esFlip2 (Espejar(Espejar d)) = True
esFlip2 (Basica d) = False
esFlip2 (Rotar d) = esRot360 d
esFlip2 (Rot45 d) = esRot360 d
esFlip2 (Espejar d) = esRot360 d
esFlip2 (Apilar _ _ d1 d2) = (esRot360 d1) || (esRot360 d2)
esFlip2 (Juntar _ _ d1 d2) = (esRot360 d1) || (esRot360 d2)
esFlip2 (Encimar d1 d2) = (esRot360 d1) || (esRot360 d2)

-- la cadena que se toma como parámetro es la descripción
-- del error.
check :: Pred (Dibujo a) -> String -> Dibujo a -> Either String (Dibujo a)
check pre err d | and (map (pre . pureDibe) (every d)) = Right d
                | otherwise = Left err

-- aplica todos los chequeos y acumula todos los errores,
-- sólo devuelve la figura si no hubo ningún error.
todoBien :: Dibujo a -> Either [String] (Dibujo a)
todoBien d | (esFlip2 d) && (esRot360 d) =
             Left ["Hay 2 espejados seguidos", "Hay 4 rotaciones seguidas"]
           | (esFlip2 d) = Left ["Hay 2 espejados seguidos"]
           | (esRot360 d) = Left ["Hay 4 rotaciones seguidas"]
           | otherwise = Right d

noRot360 :: Dibujo a -> Dibujo a
noRot360 (Rotar(Rotar(Rotar(Rotar d)))) = d
noRot360 (Basica d) = Basica d
noRot360 (Rotar d) = Rotar (noRot360 d)
noRot360 (Rot45 d) = Rot45 (noRot360 d)
noRot360 (Espejar d) = Espejar (noRot360 d)
noRot360 (Apilar n1 n2 d1 d2) = Apilar n1 n2 (noRot360 d1) (noRot360 d2)
noRot360 (Juntar n1 n2 d1 d2) = Juntar n1 n2 (noRot360 d1) (noRot360 d2)
noRot360 (Encimar d1 d2) = Encimar (noRot360 d1) (noRot360 d2)

noFlip2  :: Dibujo a -> Dibujo a
noFlip2 (Espejar(Espejar d)) = d
noFlip2 (Basica d) = Basica d
noFlip2 (Rotar d) = Rotar (noFlip2 d)
noFlip2 (Rot45 d) = Rot45 (noFlip2 d)
noFlip2 (Espejar d) = Espejar (noFlip2 d)
noFlip2 (Apilar n1 n2 d1 d2) = Apilar n1 n2 (noFlip2 d1) (noFlip2 d2)
noFlip2 (Juntar n1 n2 d1 d2) = Juntar n1 n2 (noFlip2 d1) (noFlip2 d2)
noFlip2 (Encimar d1 d2) = Encimar (noFlip2 d1) (noFlip2 d2)
