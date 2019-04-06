module Basico.Ejemplo where
import Dibujo
import Interp
import Basico.Escher

ejemplo :: Dibujo Base
ejemplo = escher 2 (Basica Triangulo1)

interpBas :: Output Base
interpBas Triangulo1 = trian1
interpBas Triangulo2 = trian2
interpBas TrianguloD = trianD
interpBas Rectangulo = rectan
