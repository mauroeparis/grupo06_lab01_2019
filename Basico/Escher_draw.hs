module Basico.Escher_draw where

import Basico.Escher
import Dibujo
import Graphics.Gloss
import Interp


myBC :: BC
myBC = BC {forma = Triangulo2, colorbas = Rose}


ejemplo :: Dibujo BC
ejemplo = escher 4 myBC
--ejemplo = escher 3 (100 100 0 255 Triangulo2) -- escher 2 (Basica Triangulo1)

-- interpColor :: Colores -> Color
-- interpColor Rojo = red
-- interpColor Azul = blue
-- interpColor Amarillo = yellow
-- interpColor Verde = green
-- interpColor Magenta = magenta
-- interpColor Cyan = cyan
-- interpColor Rose = rose
-- interpColor Violet = violet
-- interpColor Azure = azure
-- interpColor Aquamarine = aquamarine
-- interpColor Chartreuse = chartreuse
-- interpColor Orange = orange
-- interpColor Black = black
-- interpColor White = white
--
--
-- interpForm :: Base -> FloatingPic
-- interpForm Blanco = blanco
-- interpForm Triangulo1 = trian1
-- interpForm Triangulo2 = trian2
-- interpForm TrianguloD = trianD
-- interpForm Rectangulo = rectan
--
-- interpBas :: Output BC
-- interpBas bc a b c = color (interpColor (colorbas bc)) $ interpForm (forma bc) a b c
