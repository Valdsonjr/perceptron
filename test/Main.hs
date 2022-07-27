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
        nrDeEpocas = 100,
        taxaDeAprendizado = 0.01
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

    hspec $ do
        describe "Perceptron - OR" $ do
            it "p([0, 0, 0]) == 0" $
                perceptronOr [0,0,0] `shouldBe` 0
            it "p([0, 0, 1]) == 1" $
                perceptronOr [0,0,1] `shouldBe` 1
            it "p([0, 1, 0]) == 1" $
                perceptronOr [0,1,0] `shouldBe` 1
            it "p([1, 0, 0]) == 1" $
                perceptronOr [1,0,0] `shouldBe` 1
            it "p([1, 0, 1]) == 1" $
                perceptronOr [1,0,1] `shouldBe` 1
            it "p([0, 1, 1]) == 1" $
                perceptronOr [0,1,1] `shouldBe` 1
            it "p([1, 1, 0]) == 1" $
                perceptronOr [1,1,0] `shouldBe` 1
            it "p([1, 1, 1]) == 1" $
                perceptronOr [1,1,1] `shouldBe` 1

        describe "Perceptron - AND" $ do
            it "p([0, 0, 0]) == 0" $
                perceptronAnd [0,0,0] `shouldBe` 0
            it "p([0, 0, 1]) == 0" $
                perceptronAnd [0,0,1] `shouldBe` 0
            it "p([0, 1, 0]) == 0" $
                perceptronAnd [0,1,0] `shouldBe` 0
            it "p([1, 0, 0]) == 0" $
                perceptronAnd [1,0,0] `shouldBe` 0
            it "p([1, 0, 1]) == 0" $
                perceptronAnd [1,0,1] `shouldBe` 0
            it "p([0, 1, 1]) == 0" $
                perceptronAnd [0,1,1] `shouldBe` 0
            it "p([1, 1, 0]) == 0" $
                perceptronAnd [1,1,0] `shouldBe` 0
            it "p([1, 1, 1]) == 1" $
                perceptronAnd [1,1,1] `shouldBe` 1
