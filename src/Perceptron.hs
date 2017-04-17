module Perceptron (
    runP, trainP, input, initialize, initTarget, Perceptron
) where

import           Data.Matrix (Matrix, matrix, ncols, nrows, (!), fromList, fromLists, transpose)

type Perceptron = (Matrix Double, Double -> Double)
type Input = Matrix Double
type Output = Matrix Double
type Target = Matrix Double

runP :: Input -> Perceptron -> Output
runP i (w, f) = fmap f $ i * w

lr :: Double
lr = 0.01

trainP :: Input -> Target -> Word -> Perceptron -> Perceptron
trainP x t maxE (w, f)
    | maxE == 0 || w == w' = (w, f)
    | otherwise = trainP x t (maxE - 1) (w', f)
    where
        (r,c) = (nrows w, ncols w)
        y = runP x (w, f)
        w' = matrix r c upd
        upd (i, j) = foldr ((+) . upd' . flip (,) j) (w ! (i, j)) [1 .. nrows x]
        upd' k  = lr * (t ! k - y ! k) * x ! k

input :: Matrix Double -> Input
input m = matrix i (j + 1) (\k -> if snd k == j + 1 then 1 else m ! k)
    where (i, j) = (nrows m, ncols m)

-- initialize : Number of Weights -> Number of Neurons -> Activation Function -> Perceptron
initialize :: Int -> Int -> (Double -> Double) -> Perceptron
initialize r c f = (fromList (r + 1) c (ws ++ replicate c (-1)), f)
    where ws = replicate (r * c) 0

-- initTarget : Each list is the targets for a neuron
initTarget :: [[Double]] -> Target
initTarget = transpose . fromLists
