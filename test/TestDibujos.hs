module Main(main)where
import Test.HUnit
import System.Exit(exitFailure)
import Control.Monad(when)
import Dibujo 


-- Casos de prueba para la función figura
testFigura1 = TestCase (assertEqual "figura 1" (figura 1) (figura 1))
testFigura2 = TestCase (assertEqual "figura 'a'" (figura 'a') (figura 'a'))

-- Casos de prueba para la función rotar
testrotar1 = TestCase (assertEqual "rotar (figura 1)" (rotar (figura 1)) (rotar (figura 1)))
testrotar2 = TestCase (assertEqual "rotar (rotar (figura 'a'))" (rotar (rotar (figura 'a'))) (rotar (rotar (figura 'a'))))

-- Casos de prueba para la función espejar
testEspejar1 = TestCase (assertEqual "espejar (figura 1)" (espejar (figura 1)) (espejar (figura 1)))
testEspejar2 = TestCase (assertEqual "espejar (rotar (figura 'a'))" (espejar (rotar (figura 'a'))) (espejar (rotar (figura 'a'))))

-- Casos de prueba para la función rot45
testRot45_1 = TestCase (assertEqual "rot45 (figura 1)" (rot45 (figura 1)) (rot45 (figura 1)))
testRot45_2 = TestCase (assertEqual "rot45 (rotar (figura 'a'))" (rot45 (rotar (figura 'a'))) (rot45 (rotar (figura 'a'))))

-- Casos de prueba para la función apilar
testApilar1 = TestCase (assertEqual "apilar 1 2 (figura 1) (figura 2)" (apilar 1 2 (figura 1) (figura 2)) (apilar 1 2 (figura 1) (figura 2)))
testApilar2 = TestCase (assertEqual "apilar 0.5 0.5 (rotar (figura 1)) (espejar (figura 2))" (apilar 0.5 0.5 (rotar (figura 1)) (espejar (figura 2))) (apilar 0.5 0.5 (rotar (figura 1)) (espejar (figura 2))))

-- Casos de prueba para la función juntar
testJuntar1 = TestCase (assertEqual "juntar 1 2 (figura 1) (figura 2)" (juntar 1 2 (figura 1) (figura 2)) (juntar 1 2 (figura 1) (figura 2)))
testJuntar2 = TestCase (assertEqual "juntar 0.5 0.5 (rotar (figura 1)) (espejar (figura 2))" (juntar 0.5 0.5 (rotar (figura 1)) (espejar (figura 2))) (juntar 0.5 0.5 (rotar (figura 1)) (espejar (figura 2))))

-- Casos de prueba para la función encimar
testEncimar1 = TestCase (assertEqual "encimar (figura 1) (figura 2)" (encimar (figura 1) (figura 2)) (encimar (figura 1) (figura 2)))
testEncimar2 = TestCase (assertEqual "encimar (rotar (figura 1)) (espejar (figura 2))" (encimar (rotar (figura 1)) (espejar (figura 2))) (encimar (rotar (figura 1)) (espejar (figura 2))))

-- Casos de prueba para la función comp
testComp1 = TestCase (assertEqual "comp 1 (+1) 1" 2 (comp 1 (+1) 1))
testComp2 = TestCase (assertEqual "comp 1 (+1) 2" 3 (comp 1 (+1) 2))

-- Casos de prueba para la función r180
testR180_1 = TestCase (assertEqual "r180 (figura 1)" (rotar (rotar (figura 1))) (r180 (figura 1)))
testR180_2 = TestCase (assertEqual "r180 (rotar (figura 1))" (rotar (rotar (rotar (figura 1)))) (r180 (rotar (figura 1))))

-- Casos de prueba para la función r270
testR270_1 = TestCase (assertEqual "r270 (figura 1)" (rotar (rotar (rotar (figura 1)))) (r270 (figura 1)))
testR270_2 = TestCase (assertEqual "r270 (rotar (figura 1))" (rotar (rotar (rotar (rotar (figura 1))))) (r270 (rotar (figura 1))))

-- Casos de prueba para la función (.-.)
testPunto_1 = TestCase (assertEqual "figura 1 .-. figura 2" (apilar 1 1 (figura 1) (figura 2)) (figura 1 .-. figura 2))
testPunto_2 = TestCase (assertEqual "figura 1 .-. rotar (figura 2)" (apilar 1 1 (figura 1) (rotar (figura 2))) (figura 1 .-. rotar (figura 2)))

-- Casos de prueba para la función (///)
testBarra_1 = TestCase (assertEqual "figura 1 /// figura 2" (juntar 1 1 (figura 1) (figura 2)) (figura 1 /// figura 2))
testBarra_2 = TestCase (assertEqual "figura 1 /// rotar (figura 2)" (juntar 1 1 (figura 1) (rotar (figura 2))) (figura 1 /// rotar (figura 2)))

-- Casos de prueba para la función (^^^)
testCircunflejo_1 = TestCase (assertEqual "figura 1 ^^^ figura 2" (encimar (figura 1) (figura 2)) (figura 1 ^^^ figura 2))
testCircunflejo_2 = TestCase (assertEqual "figura 1 ^^^ rotar (figura 2)" (encimar (figura 1) (rotar (figura 2))) (figura 1 ^^^ rotar (figura 2)))

-- Casos de prueba para la función cuarteto
testCuarteto1 = TestCase (assertEqual "cuarteto (figura 1) (figura 2) (figura 3) (figura 4)" (apilar 1 1 (juntar 1 1 (figura 1) (figura 2)) (juntar 1 1 (figura 3) (figura 4))) (cuarteto (figura 1) (figura 2) (figura 3) (figura 4)))
testCuarteto2 = TestCase (assertEqual "cuarteto (rotar (figura 1)) (espejar (figura 2)) (rotar (espejar (figura 3))) (espejar (rotar (figura 4)))" (apilar 1 1 (juntar 1 1 (rotar (figura 1)) (espejar (figura 2))) (juntar 1 1 (rotar (espejar (figura 3))) (espejar (rotar (figura 4))))) (cuarteto (rotar (figura 1)) (espejar (figura 2)) (rotar (espejar (figura 3))) (espejar (rotar (figura 4)))))

-- Casos de prueba para la función encimar4
testEncimar41 = TestCase (assertEqual "encimar4 (figura 1)" (encimar (rotar (rotar (rotar (figura 1)))) (encimar (rotar (rotar (figura 1))) (encimar (rotar (figura 1)) (figura 1)))) (encimar4 (figura 1)))
testEncimar42 = TestCase (assertEqual "encimar4 (rotar (figura 1))" (encimar (rotar (rotar (rotar (rotar (figura 1))))) (encimar (rotar (rotar (rotar (figura 1)))) (encimar (rotar (rotar (figura 1))) (rotar (figura 1))))) (encimar4 (rotar (figura 1))))

-- Casos de prueba para la función ciclar
testCiclar1 = TestCase (assertEqual "ciclar (figura 1)" (apilar 1.0 1.0 (juntar 1.0 1.0 (figura 1) (rotar (figura 1))) (juntar 1.0 1.0 (rotar (rotar (figura 1))) (rotar (rotar (rotar (figura 1)))))) (ciclar (figura 1)))
testCiclar2 = TestCase (assertEqual "ciclar (rotar (figura 1))" (apilar 1.0 1.0 (juntar 1.0 1.0 (rotar (figura 1)) (rotar (rotar (figura 1)))) (juntar 1.0 1.0 (rotar (rotar (rotar (figura 1)))) (rotar (rotar (rotar (rotar (figura 1))))))) (ciclar (rotar (figura 1))))

-- Casos de prueba para la función foldDib
testFoldDib1 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (figura 1)" 1 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (figura 1)))
testFoldDib2 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (rotar (figura 1))" 1 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (rotar (figura 1))))
testFoldDib3 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (espejar (figura 1))" 1 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (espejar (figura 1))))
testFoldDib4 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (rot45 (figura 1))" 1 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (rot45 (figura 1))))
testFoldDib5 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (juntar 1 1 (figura 1) (figura 2))" 3 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (juntar 1 1 (figura 1) (figura 2))))
testFoldDib6 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (apilar 1 1 (figura 1) (figura 2))" 3 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (apilar 1 1 (figura 1) (figura 2))))
testFoldDib7 = TestCase (assertEqual "foldDib id id id id (_ _ a b -> a+b) (_ _ a b -> a+b) (+) (encimar (figura 1) (figura 2))" 3 (foldDib id id id id (\_ _ a b -> a+b) (\_ _ a b -> a+b) (+) (encimar (figura 1) (figura 2))))

-- Casos de prueba para la función mapDib
-- testMapDib1 = TestCase (assertEqual "mapDib figura (figura 1)" (figura 1) (mapDib figura (figura 1)))
-- testMapDib2 = TestCase (assertEqual "mapDib figura (rotar (figura 1))" (rotar (figura 1)) (mapDib figura (rotar (figura 1))))
-- testMapDib3 = TestCase (assertEqual "mapDib figura (espejar (figura 1))" (espejar (figura 1)) (mapDib figura (espejar (figura 1))))
-- testMapDib4 = TestCase (assertEqual "mapDib figura (rot45 (figura 1))" (rot45 (figura 1)) (mapDib figura (rot45 (figura 1))))
-- testMapDib5 = TestCase (assertEqual "mapDib (a -> a*a) (juntar 1 2 (figura 1) (figura 2))" (juntar 1 2 (figura 1) (figura 4)) (mapDib (\a -> figura (a*a)) (juntar 1 2 (figura 1) (figura 2))))
-- testMapDib6 = TestCase (assertEqual "mapDib (a -> a*a) (apilar 1 2 (figura 1) (figura 2))" (apilar 1 2 (figura 1) (figura 4)) (mapDib (\a -> figura (a*a)) (apilar 1 2 (figura 1) (figura 2))))
-- testMapDib7 = TestCase (assertEqual "mapDib (a -> a*a+a) (encimar (figura 1) (figura 2))" (encimar (figura 2) (figura 6)) (mapDib (\a -> figura (a*a+a)) (encimar (figura 1) (figura 2))))

-- Casos de prueba para la función figuras
testFiguras1 = TestCase (assertEqual "figuras (figura 1)" [1] (figuras (figura 1)))
testFiguras2 = TestCase (assertEqual "figuras (rotar (figura 1))" [1] (figuras (rotar (figura 1))))
testFiguras3 = TestCase (assertEqual "figuras (espejar (figura 1))" [1] (figuras (espejar (figura 1))))
testFiguras4 = TestCase (assertEqual "figuras (rot45 (figura 1))" [1] (figuras (rot45 (figura 1))))
testFiguras5 = TestCase (assertEqual "figuras (juntar 1 2 (figura 1) (figura 2))" [1,2] (figuras (juntar 1 2 (figura 1) (figura 2))))
testFiguras6 = TestCase (assertEqual "figuras (apilar 1 2 (figura 1) (figura 2))" [1,2] (figuras (apilar 1 2 (figura 1) (figura 2))))
testFiguras7 = TestCase (assertEqual "figuras (encimar (figura 1) (figura 2))" [1,2] (figuras (encimar (figura 1) (figura 2))))

runTest = runTestTT $ TestList [testFigura1, testFigura2, testrotar1, testrotar2, testEspejar1, testEspejar2, testRot45_1, testRot45_2, testApilar1, testApilar2, testEncimar1, testEncimar2, testComp1, testComp2, testR180_1, testR180_2, testR270_1, testR270_2, testPunto_1, testPunto_2, testBarra_1, testBarra_2, testCircunflejo_1, testCircunflejo_2, testCuarteto1, testCuarteto2, testEncimar41, testEncimar42, testCiclar1, testCiclar2, testFoldDib1, testFoldDib2, testFoldDib3, testFoldDib4, testFoldDib5, testFoldDib6, testFoldDib7, {-testMapDib1, testMapDib2, testMapDib3, testMapDib4, testMapDib5, testMapDib6, testMapDib7-} testFiguras1, testFiguras2, testFiguras3, testFiguras4, testFiguras5, testFiguras6, testFiguras7]

-- Ejecutar los casos de prueba
main = runTest