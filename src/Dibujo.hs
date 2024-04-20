module Dibujo ( 
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar, change,
    foldDib, mapDib,
    figuras
    ) where


-- nuestro lenguaje 
data Dibujo a = Figura a 
            | Rotar (Dibujo a )
            | Espejar (Dibujo a)
            | Rot45 (Dibujo a) 
            | Apilar Float Float (Dibujo a) (Dibujo a) 
            | Juntar Float Float (Dibujo a) (Dibujo a)
            | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp i f 
    |i== 0 = id 
    |i <0 = error "no se puede componer"
    |i >0 = f . (comp (i-1) f)


-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar


espejar :: Dibujo a -> Dibujo a
espejar = Espejar

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = Encimar a b 

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = Apilar 1.0 1.0 a b

(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = Juntar 1.0 1.0 a b

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = rotar 

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 rotar  

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 rotar

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 f = f ^^^ rotar f ^^^ r180 f ^^^ r270 f   

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto f1 f2 f3 f4 = (f1///f2) .-. (f3///f4)


-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar f = cuarteto f (rotar f) (r180 f) (r270 f ) 

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f dibujo = foldDib (Figura . f) Rotar Espejar Rot45 Apilar Juntar Encimar dibujo
-- mapDib f (Figura x) = Figura (f x)
-- mapDib f (Rotar d) = Rotar (mapDib f d)
-- mapDib f (Espejar d) = Espejar (mapDib f d)
-- mapDib f (Rot45 d) = Rot45 (mapDib f d)
-- mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
-- mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
-- mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f = foldDib f Rotar Espejar Rot45 Apilar Juntar Encimar

-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib f r45 r es a j en d  = case d of  
    Figura fig -> f fig 
    Rotar dibu -> r(foldDib f r45 r es a j en dibu) 
    Espejar dibu -> es(foldDib f r45 r es a j en dibu)
    Rot45 dibu -> r45(foldDib f r45 r es a j en dibu)
    Apilar x y dibu1 dibu2 -> a x y (foldDib f r45 r es a j en dibu1) (foldDib f r45 r es a j en dibu2)
    Juntar x y dibu1 dibu2 -> j x y (foldDib f r45 r es a j en dibu1) (foldDib f r45 r es a j en dibu2)
    Encimar dibu1 dibu2 -> en(foldDib f r45 r es a j en dibu1) (foldDib f r45 r es a j en dibu2)
-- Junta todas las figuras básicas de un dibujo.
-- mi idea seria tomar todas las figuras basicas recursivamente para ir metiendolas en un array

figuras :: Dibujo a -> [a] 
figuras = foldDib (\x -> [x]) id id id (\_ _ x y -> x ++ y) (\_ _ x y -> x ++ y) (++)
-- figuras (Figura a )= [ a ]  
-- figuras (Rotar a ) = figuras a 
-- figuras (Espejar a ) = figuras a
-- figuras (Rot45 a ) = figuras a 
-- figuras (Apilar _ _ f1 f2) = figuras f1 ++ figuras f2 
-- figuras (Juntar _ _ f1 f2 ) = figuras f1 ++ figuras f2 
-- figuras (Encimar f1 f2 ) = figuras f1 ++ figuras f2 
