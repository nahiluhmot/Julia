module Generator (generate) where

import Data.Complex (Complex((:+)), magnitude)
import Control.Parallel.Strategies (using, parList, rdeepseq)

toPoint :: Fractional a => Int -> Int -> a
toPoint s n = ((fromIntegral n * 4) / fromIntegral s) - 2


{--
escapes :: RealFloat a => Complex a -> Complex a -> Int -> Int
escapes c j it = round . (/ 2 ^ length zs) . (* fromIntegral (maxBound :: Int))  $  magnitude c
    where zs  = take it . takeWhile ((< 4) . magnitude) $ iterate (\z -> z ^ 2 + j) c
--}

escapes :: RealFloat a => Complex a -> Complex a -> Int -> Int
escapes c j it = length
               . takeWhile (\z -> magnitude z < 4)
               . take it
               . iterate (\z -> z ^ 2 + j)
               $ c

generate :: RealFloat a => Complex a -> Int -> Int -> [[Int]]
generate j s it = map (genRow . toPoint s) [0..pred s] `using` parList rdeepseq
    where genRow r = [escapes (r :+ i) j it | i <- is]
          is = map (toPoint s) [0..pred s]
