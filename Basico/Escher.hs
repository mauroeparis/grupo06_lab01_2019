-- supongamos que eligen
type Escher = Bool

-- el dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u p = apilar(apilar(p2, rot(p2)), apilar(rot(rot(p2))), rot(rot(rot(p2))))
              where p2 = rotar(rot45(p))
          -- u = over(over(fish2, rot(fish2)), over(rot(rot(fish2))), rot(rot(rot(fish2))))

-- el dibujo t
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t p = encimar(p, encimar(p2, p3))
              where p2 = rotar(rot45(p))
                    p3 = rot(rot(rot(p2)))

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto(vacia ,vacia ,vacia ,dibujo_u)
esquina 2 p = cuarteto((esquina 1 p),(lado 1 p),rot(lado 1 p),dibujo_u)
esquina n p = cuarteto(esquina (n-1 p), (lado n p), rot(lado n p), dibujo_u)

-- corner1 = quartet(blank,blank,blank,u)
-- corner2 = quartet(corner1,side1,rot(side1),u)
--
-- corner[n] = quartet(corner[n-1],side,rot(side),u)


-- lado con nivel de detalle
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto (vacia, vacia, rot(dibujo_t), dibujo_t)
lado 2 p = cuarteto((lado 1 p), (lado 1 p), rot(dibujo_t), dibujo_t)
lado n p = cuarteto(lado (n-1) p ,lado (n-1) p , rot(dibujo_t), dibujo_t)


-- side1 = quartet(blank,blank,rot(t),t)
-- side2 = quartet(side1,side1,rot(t),t)
-- side[n] = quartet(side[n-1],side[n-1],rot(t),t)

-- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x =
                  apilar(1,2,juntar(1,2,p,juntar(1,1,q,r)),
                  apilar(1,1,juntar(1,2,s,juntar(1,1,t,u)),
                  juntar(1,2,v,juntar(1,1,w,x))))

-- el dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher = undefined
