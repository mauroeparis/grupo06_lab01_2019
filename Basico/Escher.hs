module Basico.Escher where

import Dibujo

-- supongamos que eligen
type Escher = Base

-- el dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u escher = (escher2 ^^^ (rot90 escher2)) ^^^ ((r180 escher2) ^^^ (r270 escher2))
                  where escher2 = esp $ rotar45 escher

--   -- u = over(over(fish2, rot90(fish2)), over(rot90(rot90(fish2))), rot90(rot90(rot90(fish2))))

-- el dibujo t
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t escher = escher ^^^ (escher2 ^^^ escher3)
              where escher2 = esp $ rotar45 escher
                    escher3 = r270 escher2

-- t = over(fish, over(fish2, fish3))
--       where fish2 = flip(rotar45(fish))
--             fish3 = rot(rot(rot(fish2)))

-- -- lado con nivel de detalle
lado :: Float -> Dibujo Escher -> Dibujo Escher
lado 1 escher = (.-.) ((///) Vacio Vacio) ((///) (rot90(dibujo_t escher)) (dibujo_t escher))
lado 2 escher = (.-.) ((///) (lado 1 escher) (lado 1 escher)) ((///) (rot90(dibujo_t escher)) (dibujo_t escher))
lado n escher = (.-.) ((///) (lado (n-1) escher) (lado (n-1) escher)) ((///) (rot90(dibujo_t escher)) (dibujo_t escher))

-- -- side1 = quartet(blank,blank,rot90(t),t)
-- -- side2 = quartet(side1,side1,rot90(t),t)
-- -- side[n] = quartet(side[n-1],side[n-1],rot90(t),t)

-- esquina con nivel de detalle en base a la figura p
esquina :: Float -> Dibujo Escher -> Dibujo Escher
esquina 1 escher = (.-.) ((///) Vacio Vacio) ((///) Vacio (dibujo_u escher))
esquina 2 escher = (.-.) ((///) (esquina 1 escher) (lado 1 escher)) ((///) (rot90(lado 1 escher)) (dibujo_u escher))
esquina n escher = (.-.) ((///) (esquina (n-1) escher) (lado (n-1) escher)) ((///) (rot90(lado (n-1) escher)) (dibujo_u escher))

-- -- corner1 = quartet(blank,blank,blank,u)
-- -- corner2 = quartet(corner1,side1,rot90(side1),u)
-- -- corner[n] = quartet(corner[n-1],side,rot90(side),u)

-- -- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r)) (
                          Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (
                          Juntar 1 2 v (Juntar 1 1 w x)))

-- nonet(p, q, r,
--       s, t, u,
--       v, w, x) =
--                   above(1,2,beside(1,2,p,beside(1,1,q,r)),
--                   above(1,1,beside(1,2,s,beside(1,1,t,u)),
--                   beside(1,2,v,beside(1,1,w,x))))

-- -- el dibujo de Escher:
escher :: Float -> Escher -> Dibujo Escher
escher n e = noneto esquina' lado' (r270 esquina')
                  (rot90 lado') (dibujo_u escher) (esp $ rot90 lado')
                  (rot90 esquina') (r180 lado') (r180 esquina')
                  where escher = pureDibe e
                        esquina' = esquina n escher
                        lado' = lado n escher
--     -- squarelimit2 =
--     --               nonet(corner2, side2, rot90(rot90(rot90(corner2))),
--     --               rot90(side2),u,rot90(rot90(rot90(side2))),
--     --               rot90(corner2),rot90(rot90(side2)),rot90(rot90(corner2)))
