module Main where

import           Data.Matrix (fromLists)
import           Perceptron  (Perceptron, runP, trainP, input, initialize, initTarget)
import           System.Directory (getCurrentDirectory)
import           System.Filepath (Filepath, (</>))

activate :: Double -> Double
activate n = if n > 0 then 1 else -1

p :: Perceptron
p = initialize 3 1 activate

printAND :: IO ()
printAND = do
    let x = input $ fromLists [
            [1, 1, 1],
            [1, 1, 0],
            [1, 0, 1],
            [0, 1, 1]]

    let c = input $ fromLists [
            [ 1,  0,  0],
            [ 0,  1,  0],
            [ 0,  0,  1],
            [ 0,  0,  0],
            [ 1,  1,  0],
            [ 1,  0,  1],
            [ 0,  1,  1],
            [ 1,  1,  1]]

    let t = initTarget [[1,-1,-1,-1]]

    print (fst $ trainP x t 100 p)
    print (runP c $ trainP x t 100 p)

printOR :: IO ()
printOR = do
    let x = input $ fromLists [
            [0, 0, 1],
            [0, 1, 0],
            [1, 0, 0],
            [0, 0, 0]]

    let c = input $ fromLists [
            [ 1,  0,  0],
            [ 0,  1,  0],
            [ 0,  0,  1],
            [ 0,  0,  0],
            [ 1,  1,  0],
            [ 1,  0,  1],
            [ 0,  1,  1],
            [ 1,  1,  1]]

    let t = initTarget [[1,1,1,-1]]

    print (fst $ trainP x t 100 p)
    print (runP c $ trainP x t 100 p)

printCHARS :: IO ()
printCHARS = do
    dir <- getCurrentDirectory
    let trainingSET = dir </> "treino"
    let testingSET = dir </> "teste"
    
    getDirectoryContents trainingSET >>= print
    getDirectoryContents testingSET >>= print
    print dir


main :: IO ()
main =
    printCHARS
