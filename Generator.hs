{-# LANGUAGE BangPatterns #-}

module Generator (generate) where

import Data.Complex (Complex((:+)), realPart, imagPart)
import Control.Parallel.Strategies (using, parList, rdeepseq)

toPoint :: Fractional a => Int -> Int -> a
toPoint !s !n = ((fromIntegral n * 4) / fromIntegral s) - 2

escapes :: RealFloat a => Complex a -> Complex a -> Int -> Int
escapes !c !j !it = length ys
    where xs = take it . iterate (\z -> z ^ 2 + j) $ c
          ys = takeWhile trapped xs
          trapped x = (abs . realPart $ x) < 2.0 && 
                      (abs . imagPart $ x) < 2.0

generate :: RealFloat a => Complex a -> Int -> Int -> [[Int]]
generate !j !s !it = graph'
    where genRow x = [escapes (toPoint s x :+ toPoint s y) j it | y <- [0..s]]
          graph    = map genRow [0..s]
          graph'   = graph `using` parList rdeepseq
