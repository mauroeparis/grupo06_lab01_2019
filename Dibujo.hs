module Dibujo where
-- definir el lenguaje

-- data Basica a = trian1 a b c | trian2 a | trianD a | rectan a

data Dibujo a = Basica a
                | Rotar (Dibujo a)
                | Espejar (Dibujo a)
                | Rot45 (Dibujo a)
                | Apilar Int Int (Dibujo a) (Dibujo a)
                | Juntar Int Int (Dibujo a) (Dibujo a)
                | Encimar (Dibujo a) (Dibujo a)

-- composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> (a -> a)
comp f 0 = f
comp f n = comp f (n-1) . f

-- comp :: ((a -> a) -> Int -> a -> a
-- (comp _ 0 a )= a
-- comp fun n a = comp fun (n-1) (fun a)

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
