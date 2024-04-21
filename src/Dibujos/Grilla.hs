module Dibujos.Grilla (
    grilla,
    grillaConf
) where

import Dibujo (Dibujo, juntar, apilar, figura)
import FloatingPic(Conf(..), Output, half, zero, vacia)
import Graphics.Gloss (Picture, Vector, blank, line, pictures, rotate, translate, text, scale)

type Pair = (Int, Int)

-- drawPair :: Pair -> Picture
-- drawPair (x, y) = text $ show (x, y)  

drawNumber :: Pair -> Picture
drawNumber (x, y) = translate (fromIntegral y * 100) (fromIntegral (7 - x) * 100) $ scale 0.1 0.1 (text $ show (x, y)) 

drawPair :: Picture
drawPair =  translate (5) 40 $ pictures [drawNumber (x, y) | x <- [0..7], y <- [0..7]]

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar (fromIntegral $ length ds) 1 d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar (fromIntegral $ length ds) 1 d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

interBasGrilla :: Output Picture
interBasGrilla a _ _ _ = pictures[a] 

testAll :: Dibujo Picture
testAll = grilla [[figura drawPair]]

grillaConf :: Conf
grillaConf = Conf {
    name = "grilla"
    , pic = testAll
    , bas = interBasGrilla
}

