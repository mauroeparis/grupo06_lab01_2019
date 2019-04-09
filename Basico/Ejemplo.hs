module Basico.Ejemplo where
import Dibujo
import Interp
import Basico.Escher

ejemplo :: Dibujo Base
ejemplo = escher 3 Triangulo2 -- escher 2 (Basica Triangulo1)

interpBas :: Output Base
interpBas Blanco = blanco
interpBas Triangulo1 = trian1
interpBas Triangulo2 = fShape
interpBas TrianguloD = trianD
interpBas Rectangulo = rectan
