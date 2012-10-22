module Generator (generate) where

import Foreign.C.Types (CInt)
import Data.Complex (Complex((:+)), magnitude)
import Control.Parallel.Strategies (using, parList, rdeepseq)

toPoint :: Fractional a => Int -> Int -> a
toPoint s n = ((fromIntegral n * 4) / fromIntegral s) - 2

escapes :: RealFloat a => Complex a -> Complex a -> Int -> Int
escapes c j it = round $  (1 / 4) * (fromIntegral (maxBound :: CInt)) * (log ra / (2 ** v))
    where zs = takeWhile ((< ra) . magnitude) . take it . iterate ((+ j) . (^ 2)) $ c
          v  = fromIntegral (length zs) - log (log (magnitude . last $ zs) / log ra)
          ra = fromIntegral (maxBound :: CInt)

generate :: RealFloat a => Complex a -> Int -> Int -> [[Int]]
generate j s it = map (genRow . toPoint s) [0..pred s] `using` parList rdeepseq
    where genRow r = [escapes (r :+ i) j it | i <- is]
          is = map (toPoint s) [0..pred s]
