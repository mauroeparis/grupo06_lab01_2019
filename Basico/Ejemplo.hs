module Basico.Ejemplo where
import Dibujo
import Interp

data Base = Triangulo1
            | Triangulo2
            | TrianguloD
            | Rectangulo

ejemplo :: Dibujo Base
ejemplo = Basica Triangulo1

interpBas :: Output Base
interpBas Triangulo1 = trian1
interpBas Triangulo2 = trian2
interpBas TrianguloD = trianD
interpBas Rectangulo = rectan
