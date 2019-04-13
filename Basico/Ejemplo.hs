module Basico.Ejemplo where
import Dibujo
import Interp
import Graphics.Gloss

-- data Basica a = trian1 a b c | trian2 a | trianD a | rectan a
data Base = Blanco
            | Triangulo1
            | Triangulo2
            | TrianguloD
            | Rectangulo

data BC = BC {
              forma :: Base,
              colorbas :: Colores
             }

ejemplo :: Dibujo BC
ejemplo = Basica (BC {forma = Triangulo1, colorbas = Amarillo})
--ejemplo = escher 3 (100 100 0 255 Triangulo2) -- escher 2 (Basica Triangulo1)

interpColor :: Colores -> Color
interpColor Rojo = red
interpColor Azul = blue
interpColor Amarillo = yellow
interpColor Verde = green
interpColor Magent = magenta
interpColor Cya = cyan


interpForm :: Base -> FloatingPic
interpForm Blanco = blanco
interpForm Triangulo1 = trian1
interpForm Triangulo2 = trian2
interpForm TrianguloD = trianD
interpForm Rectangulo = rectan

interpBas :: Output BC -- -> FloatingPic
interpBas bc a b c = color (interpColor (colorbas bc)) $ interpForm (forma bc) a b c

-- interpBas :: Output Base
-- interpColor Blanco = blanco
-- interpColor Triangulo1 = (trian1)
-- interpColor Triangulo2 = (trian2)
-- interpColor TrianguloD = (trianD)
-- interpColor Rectangulo = (rectan)
