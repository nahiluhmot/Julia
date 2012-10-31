import Generator (generate)
import System (getArgs)
import Data.Complex (Complex((:+)))
import Foreign.C.Types (CInt)
import Graphics.GD (newImage, setPixel, savePngFile, withImage, Image, Color)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import Control.Monad (zipWithM_)

imageFromRaster :: Integral a => [[a]] -> IO Image
imageFromRaster matrix = do
    let width  = length matrix
        height = length $ head matrix
    image  <- newImage (width, height)
    zipWithM_
        (\row x -> 
            zipWithM_ 
                (\c y ->
                    setPixel (x, y) (fromIntegral c) image)
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
        ras  = (generate size iter (real :+ imag) :: [[Int]]) `using` parList rdeepseq
    imageFromRaster ras `withImage` savePngFile "julia.png"
