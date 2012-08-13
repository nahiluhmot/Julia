{-# LANGUAGE BangPatterns #-}

module Generator (generate) where

import Data.Complex (Complex((:+)), magnitude)
import Control.Parallel.Strategies (using, parList, rdeepseq)

toPoint :: Fractional a => Int -> Int -> a
toPoint !s !n = ((fromIntegral n * 4) / fromIntegral s) - 2

escapes :: RealFloat a => Complex a -> Complex a -> Int -> Int
escapes !c !j !it =
    length .
        takeWhile (\x -> magnitude x < 4) .
            take it $
                iterate (\z -> z ^ 2 + j) c

generate :: RealFloat a => Complex a -> Int -> Int -> [[Int]]
generate !j !s !it = map genRow [0..s] `using` parList rdeepseq
    where genRow !x = [escapes (toPoint s x :+ toPoint s y) j it | !y <- [0..s]]
