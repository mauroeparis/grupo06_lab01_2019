module Basico.Escher where

import Dibujo
import Graphics.Gloss
import Interp

data Base = Blanco
            | Triangulo1
            | Triangulo2
            | TrianguloD
            | Rectangulo
            | Curva

data Colores = Rojo
             | Azul
             | Verde
             | Amarillo
             | Magenta
             | Cyan
             | Rose
             | Violet
             | Azure
             | Aquamarine
             | Chartreuse
             | Orange
             | Black
             | White

data BC = BC {
              forma :: Base,
              colorbas :: Colores
             }

type Escher = BC

interpColor :: Colores -> Color
interpColor Rojo = red
interpColor Azul = blue
interpColor Amarillo = yellow
interpColor Verde = green
interpColor Magenta = magenta
interpColor Cyan = cyan
interpColor Rose = rose
interpColor Violet = violet
interpColor Azure = azure
interpColor Aquamarine = aquamarine
interpColor Chartreuse = chartreuse
interpColor Orange = orange
interpColor Black = black
interpColor White = white

interpForm :: Base -> FloatingPic
interpForm Blanco = blanco
interpForm Triangulo1 = trian1
interpForm Triangulo2 = trian2
interpForm TrianguloD = trianD
interpForm Rectangulo = rectan
interpForm Curva = curvita

interpBas :: Output BC
interpBas bc a b c = color (interpColor (colorbas bc)) $ interpForm (forma bc)
                     a b c

myBC :: BC
myBC = BC {forma = Triangulo2, colorbas = Azure}

dibujo_escher :: Dibujo BC
dibujo_escher = escher 5 myBC


------------ CONSTRUCCION DEL DIBUJO DE ESCHER ------------

-- el dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u escher = (escher2 ^^^ (rot90 escher2)) ^^^ ((r180 escher2) ^^^
                  (r270 escher2))
                  where escher2 = esp $ rotar45 escher

-- el dibujo t
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t escher = escher ^^^ (escher2 ^^^ escher3)
              where escher2 = esp $ rotar45 escher
                    escher3 = r270 escher2

-- -- lado con nivel de detalle
lado :: Float -> Dibujo Escher -> Dibujo Escher
lado 1 escher = (.-.) ((///) Vacio Vacio) ((///) (rot90(dibujo_t escher))
                (dibujo_t escher))
lado 2 escher = (.-.) ((///) (lado 1 escher) (lado 1 escher)) ((///)
                (rot90(dibujo_t escher)) (dibujo_t escher))
lado n escher = (.-.) ((///) (lado (n-1) escher) (lado (n-1) escher))
                ((///) (rot90(dibujo_t escher)) (dibujo_t escher))

-- esquina con nivel de detalle en base a la figura p
esquina :: Float -> Dibujo Escher -> Dibujo Escher
esquina 1 escher = (.-.) ((///) Vacio Vacio) ((///) Vacio (dibujo_u escher))
esquina 2 escher = (.-.) ((///) (esquina 1 escher) (lado 1 escher)) ((///)
                   (rot90(lado 1 escher)) (dibujo_u escher))
esquina n escher = (.-.) ((///) (esquina (n-1) escher) (lado (n-1) escher))
                   ((///) (rot90(lado (n-1) escher)) (dibujo_u escher))

-- -- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r)) (
                          Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (
                          Juntar 1 2 v (Juntar 1 1 w x)))

-- -- el dibujo de Escher:
escher :: Float -> Escher -> Dibujo Escher
escher n e = noneto esquina' lado' (r270 esquina')
                  (rot90 lado') (dibujo_u escher) (esp $ rot90 lado')
                  (rot90 esquina') (r180 lado') (r180 esquina')
                  where escher = pureDibe e
                        esquina' = esquina n escher
                        lado' = lado n escher
