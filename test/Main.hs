module Main where

import Perceptron
    ( Perceptron, classificar, treinar, perceptron)
-- import System.Directory ( getCurrentDirectory, getDirectoryContents )
-- import System.FilePath ( (</>) )

p :: Perceptron
p = perceptron [0, 0, 0]

printAND :: IO ()
printAND = do
    let conjuntoDeTreino =
            [ ([1, 1, 1], 1)
            , ([1, 1, 0], 0)
            , ([1, 0, 1], 0)
            , ([0, 1, 1], 0)
            ]

    let amostras =
            [ [1, 0, 0]
            , [0, 1, 0]
            , [0, 0, 1]
            , [0, 0, 0]
            , [1, 1, 0]
            , [1, 0, 1]
            , [0, 1, 1]
            , [1, 1, 1]
            ]

    let taxaDeAprendizado = 0.1
    let redeTreinada = treinar conjuntoDeTreino 100 taxaDeAprendizado p
    print redeTreinada
    mapM_ (showAmostra redeTreinada) amostras

showAmostra :: Perceptron -> [Double] -> IO ()
showAmostra rede amostra = do
    let resultado = classificar amostra rede
    putStrLn $ "Entrada: " <> show amostra <> " => " <> show resultado


printOR :: IO ()
printOR = do
    let conjuntoDeTreino =
            [ ([0, 0, 1], 1)
            , ([0, 1, 0], 1)
            , ([1, 0, 0], 1)
            , ([0, 0, 0], 0)
            ]

    let amostras =
            [ [ 1, 0, 0]
            , [ 0, 1, 0]
            , [ 0, 0, 1]
            , [ 0, 0, 0]
            , [ 1, 1, 0]
            , [ 1, 0, 1]
            , [ 0, 1, 1]
            , [ 1, 1, 1]
            ]

    let taxaDeAprendizado = 0.1
    let redeTreinada = treinar conjuntoDeTreino 100 taxaDeAprendizado p
    print redeTreinada
    mapM_ (showAmostra redeTreinada) amostras

-- printCHARS :: IO ()
-- printCHARS = do
--     dir <- getCurrentDirectory
--     let trainingSET = dir </> "treino"
--     let testingSET = dir </> "teste"

--     getDirectoryContents trainingSET >>= print
--     getDirectoryContents testingSET >>= print
--     print dir

main :: IO ()
main = printAND
    >> printOR
