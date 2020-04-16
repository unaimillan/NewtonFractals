module Newton where

import           Newton.Render (renderBasins, simpleColoring)

-- | Newton fractal for z^5 - 1 = 0.
fractal :: FilePath -> IO ()
fractal path =
  renderBasins
    path
    512
    (\z -> z ^ 8 + 15 * z ^ 4 - 16)
    (\z -> 8 * z ^ 7 + 45 * z ^ 3)
    simpleColoring

main :: IO ()
main = fractal "sample.png"
