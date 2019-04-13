module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basico.Escher_draw as E
import qualified Basico.Escher as KK

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }

ej x y = Conf {
                basic = KK.interpBas
              , fig = E.ejemplo
              , width = x
              , height = y
              }

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf KK.BC) -> IO ()
initial cf = cf >>= \cfg ->
                  let x  = width cfg
                      y  = height cfg
                  in display win white -- . withGrid
                      $ interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
  where withGrid p = pictures [p, color grey $ grid 10 (0,0) 10 100]
        grey = makeColorI 120 120 120 120

win = InWindow "Nice Window" (500, 500) (0, 0)
main = initial $ return (ej 250 250)
