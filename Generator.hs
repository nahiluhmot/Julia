module Generator (generate) where

import Data.Complex (Complex((:+)), magnitude)

toPoint :: Fractional a => Int -> Int -> a
toPoint s n = ((fromIntegral n * 4) / fromIntegral s) - 2

escapes :: (RealFloat a, Num b) => Int -> Complex a -> Complex a -> b
escapes it j = fromIntegral
             . length
             . takeWhile ((< 4.0) . magnitude)
             . take it 
             . iterate (\z -> z ^ 2 + j) 

generate :: (RealFloat a, Num b) => Int -> Int -> Complex a -> [[b]]
generate s it j = do
    let domain = map (toPoint s) [0 .. pred s]
    r <- domain
    return $ do
        i <- domain
        return $ escapes it j (r :+ i) 
