module Dibujo where
import Interp

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
comp :: ((a -> a) -> Int -> (a -> a)
comp f 0 = f
comp f n = comp f (n-1) . f

-- comp :: ((a -> a) -> Int -> a -> a
-- (comp _ 0 a )= a
-- comp fun n a = comp fun (n-1) (fun a)

-- -- rotaciones de múltiplos de 90.
-- r180 :: Dibujo a -> Dibujo a
-- comp rot90 2
-- r270 :: Dibujo a -> Dibujo a
--
-- -- Pone una figura sobre la otra, ambas ocupan el mismo espacio
-- (.-.) :: Dibujo a -> Dibujo a -> Dibujo a
--
-- -- Pone una figura al lado de la otra, ambas ocupan el mismo espacio
-- (///) :: Dibujo a -> Dibujo a -> Dibujo a
--
-- -- Superpone una figura con otra
-- (^^^) :: Dibujo a -> Dibujo a -> Dibujo a
--
-- -- dada una figura la repite en cuatro cuadrantes
-- cuarteto :: Dibujo a -> Dibujo a
--
-- -- una figura repetida con las cuatro rotaciones, superimpuestas.
-- encimar4 :: Dibujo a -> Dibujo a
--
-- -- cuadrado con la misma figura rotada `i` por `90` para `i \in \{1..3\}`.
-- -- No confundir con encimar4!
-- ciclar :: Dibujo a -> Dibujo a
