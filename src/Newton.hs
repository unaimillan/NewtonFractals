module Newton where

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Newton.Render

-- | Newton fractal for z^5 - 1 = 0.
fractal :: FilePath -> IO ()
fractal path =
  saveBasinsPic path $
  --brightnessRGB8 15 $
  renderBasins
    256
    (\z -> z ^ 8 + 15 * z ^ 4 - 16)
    (\z -> 8 * z ^ 7 + 45 * z ^ 3)
    simpleColoring

-- | Newton fractal for z^5 - 1 = 0.
fractalAnim :: FilePath -> IO ()
fractalAnim path = saveBasinsAnim path 4 pics
  where
    shifts = take 50 (iterate (+ 1) 0)
    pics =
      parMap
        rdeepseq
        (\dx ->
           renderBasins
             512
             (\z -> z ^ 8 + 15 * z ^ 4 - 4 - dx)
             (\z -> 8 * z ^ 7 + 45 * z ^ 3)
             simpleColoring)
        shifts

solution1 :: IO ()
solution1 = fractal "sample.png"

solution2 :: IO ()
solution2 = fractalAnim "sample.gif"

main :: IO ()
main = solution1
