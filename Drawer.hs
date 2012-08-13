import Generator (generate)
import System (getArgs)
import Data.Complex (Complex((:+)))
import Graphics.GD (newImage, setPixel, savePngFile, Image, Color)
import Control.Monad (zipWithM)

imageFromRaster :: [[Color]] -> IO Image
imageFromRaster matrix = do
    let width  = length matrix
        height = length $ head matrix
    image  <- newImage (width, height)
    zipWithM (\row x -> 
                 zipWithM (\c y ->  setPixel (x, y) c image)
                          row
                          [0..pred height])
             matrix
             [0..pred width]
    return image

main :: IO ()
main = do
    args <- getArgs
    let real = read (args !! 0) :: Double
        imag = read (args !! 1) :: Double
        size = read (args !! 2) :: Int
        iter = read (args !! 3) :: Int
        rast = map (map (fromIntegral . (* (2147483648 `div` iter))))
                   (generate (real :+ imag) size iter)
    image <- imageFromRaster rast
    savePngFile "julia.png" image
