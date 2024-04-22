module Dibujos.Grilla (
    grilla,
    grillaConf
) where

import Dibujo (Dibujo, juntar, apilar, figura)
import FloatingPic(Conf(..), Output)
import Graphics.Gloss (Picture, pictures, translate, text, scale)

type Pair = (Int, Int)
--(fromIntegral y * 50) (fromIntegral x * 50)
drawNumber :: Pair -> Picture
drawNumber (x, y) = translate (fromIntegral y * 105) (fromIntegral x * 105) $ scale 0.2 0.2 (text $ show (x, y))


drawPair :: Picture
drawPair =  translate 0 0 $ pictures [drawNumber (x, y) | x <- [0..7], y <- [0..7]]

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
    name = "Grilla"
    , pic = testAll
    , bas = interBasGrilla
}



