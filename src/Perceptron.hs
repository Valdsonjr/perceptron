module Perceptron where

newtype Perceptron = Perceptron { pesos :: [Double] }
    deriving (Eq, Show)

perceptron :: [Double] -> Perceptron
perceptron ps = Perceptron (-1 : ps)

classificar :: [Double] -> Perceptron -> Double
classificar amostra = ativacao . sum . zipWith (*) (1 : amostra) . pesos
    where ativacao n = if n >= 0 then 1 else 0

treinar :: [([Double], Double)] -> Word -> Double -> Perceptron -> Perceptron
treinar amostras nrEpocas taxaDeAprendizado p
    | nrEpocas == 0 || p == novoP = p
    | otherwise = treinar amostras (nrEpocas - 1) taxaDeAprendizado novoP
    where
    novoP = foldr atualizar p amostras
    atualizar (amostra, resultadoEsperado) rede =
        let resultadoObtido = classificar amostra rede
            erro = resultadoEsperado - resultadoObtido
        in
        Perceptron $
            zipWith (\w a -> w + a * taxaDeAprendizado * erro)
            (pesos rede)
            (1 : amostra)
