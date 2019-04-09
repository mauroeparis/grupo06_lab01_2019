module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

type FloatingPic = Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

-- comprender esta función es un buen ejercicio.
hlines :: Vector -> Float -> Float -> [Picture]
hlines v@(x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)]

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l

-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

blanco :: FloatingPic
blanco a b c = Blank

cuadrado :: FloatingPic
cuadrado a b c = line $ map (a V.+) [zero, c, (c V.+ b), b, zero]

simple :: Picture -> FloatingPic
simple p _ _ _ = p

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) a b c  = translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate ang $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)

-- type Output a = (a -> Vector -> Vector -> Vector -> Picture) -> (Dibujo a -> Vector -> Vector -> Vector -> Picture)
-- interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
-- interp :: Output a -> Output (Dibujo a)
interp :: (a -> FloatingPic) -> Dibujo a -> (FloatingPic)
interp f d v1 v2 v3 = sem f rotar espejar rot45 apilar juntar encimar blanco d v1 v2 v3

rotar :: (FloatingPic) -> FloatingPic
rotar fpic (a, b) (c, d) (e, f) = fpic ((a + c), (b + d)) (e, f) (-c, -d)

rot45 :: (FloatingPic) -> FloatingPic
-- p(a+(b+c)/2,(b+c)/2,(c-b)/2
rot45 fpic a b c = fpic (a V.+ half (b V.+ c))
                        (half (b V.+ c))
                        (half (c V.+ (V.negate b)))
-- rot45 fpic (a, b) (c, d) (e, f) = fpic
--                                   (a + (c + e)/2, b + (d + f)/2)
--                                   ((c + e)/2, (d + f)/2)
--                                   ((e - c)/2, (f - d)/2)

espejar :: (FloatingPic) -> FloatingPic
espejar fpic (a, b) (c, d) (e, f) = fpic ((a + c, b + d)) (-c, -d) (e, f)

encimar :: (FloatingPic) -> (FloatingPic) -> FloatingPic
encimar fpic1 fpic2 a b c = pictures [fpic1 a b c , fpic2 a b c]

juntar :: Float -> Float -> (FloatingPic) -> (FloatingPic) -> FloatingPic
juntar n1 n2 fpic1 fpic2 (a, a') (b, b') (c, c') = pictures [fpic1 (a, a') (b*n1/(n2+n1), b'*n1/(n2+n1)) (c, c'), fpic2 (a + (b*n1/(n2+n1)), a' + b'*n1/(n2+n1)) ((b * n2/(n1 + n2)), (b' * n2/(n1 + n2))) (c, c')]

apilar :: Float -> Float -> (FloatingPic) -> (FloatingPic) -> FloatingPic
apilar n m p q a b c = pictures [p (a V.+ c'') b c', q a b c'']
    where r' = n / (n + m)
          r = m / (n + m)
          c' = r' V.* c
          c'' = r V.* c

-- apilar n1 n2 fpic1 fpic2 (a, a') (b, b') (c, c') = pictures [fpic1 (a + (b*n1/(n2+n1)), a' + b'*n1/(n2+n1)) ((b * n1/(n1 + n2)), (b' * n1/(n1 + n2))) (c, c'), fpic2 (a, a') (b, b') ((b * n1/(n1 + n2)), (b' * n1/(n1 + n2)))]
