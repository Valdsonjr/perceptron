module Main where

import Perceptron
    ( ConfiguracaoTreinamento (..)
    , Perceptron
    , classificar
    , perceptron
    , treinar
    )
import Test.Hspec ( describe, hspec, it, shouldBe )

configuracao :: ConfiguracaoTreinamento
configuracao = Config {
        _nrEpocas = 100,
        _taxaAprendizado = 0.01
    }

treinarPerceptronAnd :: Perceptron -> Perceptron
treinarPerceptronAnd = treinar configuracao
    [ ([1, 1, 1], 1)
    , ([0, 1, 0], 0)
    , ([1, 0, 0], 0)
    , ([0, 1, 1], 0)
    ]

treinarPerceptronOr :: Perceptron -> Perceptron
treinarPerceptronOr = treinar configuracao
    [ ([0, 0, 1], 1)
    , ([0, 1, 0], 1)
    , ([1, 0, 0], 1)
    , ([0, 0, 0], 0)
    ]

main :: IO ()
main = do
    let p = perceptron [0, 0, 0]
    let perceptronOr = classificar $ treinarPerceptronOr p
    let perceptronAnd = classificar $ treinarPerceptronAnd p
    let tname input expected f =
            it (show input <> " should be " <> show expected) $
                f input `shouldBe` expected

    hspec $ do
        describe "Perceptron - OR" $ do
            tname [0,0,0] 0 perceptronOr
            tname [1,0,0] 1 perceptronOr
            tname [0,1,0] 1 perceptronOr
            tname [0,0,1] 1 perceptronOr
            tname [1,1,0] 1 perceptronOr
            tname [1,0,1] 1 perceptronOr
            tname [0,1,1] 1 perceptronOr
            tname [1,1,1] 1 perceptronOr

        describe "Perceptron - AND" $ do
            tname [0,0,0] 0 perceptronAnd
            tname [1,0,0] 0 perceptronAnd
            tname [0,1,0] 0 perceptronAnd
            tname [0,0,1] 0 perceptronAnd
            tname [1,1,0] 0 perceptronAnd
            tname [1,0,1] 0 perceptronAnd
            tname [0,1,1] 0 perceptronAnd
            tname [1,1,1] 1 perceptronAnd
