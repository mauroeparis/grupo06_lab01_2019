module Basico.Ejemplo where
import Dibujo
import Interp

data Base = Triangulo1

ejemplo :: Dibujo Base
ejemplo = Basica Triangulo1

interpBas :: Output Base
interpBas Triangulo1 = trian1
