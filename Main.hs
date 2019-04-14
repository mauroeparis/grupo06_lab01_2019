module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
-- import qualified Basico.Escher_draw as E
import qualified Basico.Escher as E

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }

ej x y = Conf {
                basic = E.interpBas
              , fig = E.dibujo_escher
              , width = x
              , height = y
              }

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf E.BC) -> IO ()
initial cf = cf >>= \cfg ->
                  let x  = width cfg
                      y  = height cfg
                  in display win black -- . withGrid
                      $ interp (basic cfg) (fig cfg) (-350,-350) (x,0) (0,y)
  where withGrid p = pictures [p, color grey $ grid 10 (0,0) 10 100]
        grey = makeColorI 120 120 120 120

win = InWindow "Nice Window" (750, 750) (0, 0)
main = initial $ return (ej 700 700)
