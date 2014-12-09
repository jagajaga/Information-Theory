{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List              as L
import           Data.Map
import           System.Environment

-- Подсчет количества подпоследовательностей длины n
countSequences :: Int -> String -> Map String Int
countSequences n = fromListWith (+) . flip zip (repeat 1) . subsequencesN n

subsequencesN :: Int -> [a] -> [[a]]
subsequencesN n xs = [ take n x | x <- L.tails xs, length x >= n ]

main :: IO ()
main = do
    [file] <- getArgs
    fileContents <- readFile file
    let inputSize = fromIntegral (length fileContents) :: Double
    mapM_ putStrLn $ do
        n <- [1.0..4.0] :: [Double]
        let sequences = toList $ countSequences (round n) fileContents
        let size      = length sequences
        let totalSeq  = inputSize - n + 1.0
        let missCnt   = fromIntegral (255^round n - size) :: Double
        let missProb  = 1.0 / (inputSize ^ round n)
        let existProb = 1.0 - missCnt * missProb
        let eNorm     = -missCnt * missProb
        let eZero     = 0.0 :: Double
        let coef      = [ (i, j) | (seq, len) <- sequences,
                let i = (fromIntegral len :: Double) / totalSeq, 
                let j = i * existProb ]
        let (a, b)    = Prelude.foldl (\(a,b) (c,d) -> (a - c * logBase 2 c, b - d * logBase 2 d)) (eZero,eNorm) coef
        return $ show (round n, a / n * 3721, b / n * 3721)


