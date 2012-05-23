
import Data.Complex 
import Control.Parallel (par)


coordinateToComplex w h x y = (((x * 4) / w) - 2) :+
                              (((y * 4) / h) - 2)

escapes c j it = length ys
      where xs = take it $ iterate (\z -> z ^ 2 + j) c
            ys = takeWhile (\x -> ((abs realPart x) < 2.0)  && 
                                  ((abs imagPart x) < 2.0)) xs

generate j w h it = map (\x -> 
                        map (\y -> 
                            escapes j (coordinateToComplex w h x y) it)
                            [0..h])
                        [0..w]  
