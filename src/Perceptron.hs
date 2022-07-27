{-# LANGUAGE RecordWildCards #-}
module Perceptron where

newtype Perceptron = Perceptron { pesos :: [Double] }
    deriving (Eq, Show)

data ConfiguracaoTreinamento = Config {
    nrDeEpocas :: Word,
    taxaDeAprendizado :: Double
}

perceptron :: [Double] -> Perceptron
perceptron ps = Perceptron (-1 : ps)

classificar :: Perceptron -> [Double] -> Double
classificar p amostra = ativacao . sum . zipWith (*) (1 : amostra) $ pesos p
    where ativacao n = if n >= 0 then 1 else 0

treinar :: ConfiguracaoTreinamento -- ^ Configuração de treinamento do perceptron
  -> [([Double], Double)] -- ^ Lista de amostras e resultados esperados
  -> Perceptron -- ^ Rede perceptron não treinada
  -> Perceptron
treinar Config{..} amostras p
    | nrDeEpocas == 0 || p == novoP = p
    | otherwise = treinar config amostras novoP
    where
    config = Config (nrDeEpocas - 1) taxaDeAprendizado
    novoP = foldr atualizar p amostras
    atualizar (amostra, resultadoEsperado) rede =
        let resultadoObtido = classificar rede amostra
            erro = resultadoEsperado - resultadoObtido
        in
        Perceptron $
            zipWith (\w a -> w + a * taxaDeAprendizado * erro)
            (pesos rede)
            (1 : amostra)
