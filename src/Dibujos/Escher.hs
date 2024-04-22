module Dibujos.Escher (
    escherConf
) where
import Dibujo
import Interp(interp)  
import FloatingPic(Conf(..), Output, half, zero, vacia)
import Graphics.Gloss (pictures ,  Picture (Blank ) , line , polygon) 
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

type Escher = Bool

blank :: Dibujo Escher 
blank = figura False 

-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 p'
     where
        p' = espejar (rot45 p)

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar p'(r270 p'))
    where
        p' = espejar (rot45 p)

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto blank blank blank  (dibujoU p) 
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1)p ) (rotar(lado (n-1) p )) (dibujoU p)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto blank blank (rotar(dibujoT p)) (dibujoT p ) 
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar (dibujoT p)) (dibujoT p)
{- en esta definicion lo hago con , o lo hago con () grandes dudas de la vida-}

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = grilla [[p, q, r], [s, t, u], [v, w, x]]

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n f =  noneto p q r s t u v w x where 
    p = esquina n (figura f) 
    q = lado n (figura f)
    r = r270(esquina n (figura f))
    s = rotar (lado n (figura f))
    t = dibujoU (figura f)
    u = r270(lado n (figura f))
    v = rotar (esquina n (figura f))
    w = r180(lado n (figura f))
    x = r180 (esquina n (figura f))

interpBas :: Output Escher 
interpBas False _ _ _ = Blank 
interpBas True a b c = pictures [line $ triangulo a b c, cara a b c]
  where
      triangulo a b c = map (a V.+) [zero, c, b, zero]
      cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)

escherConf :: Conf 
escherConf = Conf {
    name = "Escher" , 
    pic = escher 10 True,
    bas = interpBas 
} 
