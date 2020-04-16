module Newton.Render where

-- this module comes from JuicyPixels package
import qualified Codec.Picture as Juicy
import           Data.Complex
import           Newton.Utils  (root)

-- | Render attraction basins for zeros (roots) of a complex function
-- as a PNG image and save it as a file.
renderBasins ::
     (Fractional a, RealFloat a)
  => FilePath -- ^ Where to write the image file.
  -> Int -- ^ Size of the square image in pixels
  -> (Complex a -> Complex a) -- ^ A function f to find zero for.
  -> (Complex a -> Complex a) -- ^ A derivative of function f.
  -> (Complex a -> Juicy.PixelRGB16) -- ^ How to color roots.
  -> IO ()
renderBasins path commonSize f f' color = Juicy.writePng path image
  where
    image = Juicy.generateImage render width height
    width = commonSize -- width in pixels
    height = commonSize -- height in pixels
    (xfrom, xto) = (-2, 2) -- range for the real axis
    (yfrom, yto) = (-2, 2) -- range for the imaginary axis
    -- | Convert pixel coordinates to a complex number.
    pixelToComplex (i, j) = re :+ im
      where
        re = xfrom + (xto - xfrom) * fromIntegral i / fromIntegral width
        im = yfrom + (yto - yfrom) * fromIntegral j / fromIntegral height
    n = 100 -- maximum number of iterations
    p e = magnitude e < 1e-7 -- precision tolerance
    -- | Convert actual number of interations into
    -- into a grey pixel using logarithmic scale
    -- (this helps bring out more details in the image).
    logscale :: Integral a => Int -> a -> a
    logscale k c = floor (fromIntegral c * (1 - logBase n' k'))
      where
        n' = fromIntegral n + 1
        k' = fromIntegral k + 1
    -- | Render one pixel.
    render :: Int -> Int -> Juicy.PixelRGB16
    render i j =
      case root n p f f' (pixelToComplex (i, j)) of
        Nothing -> Juicy.PixelRGB16 0 0 0 -- black
        Just (k, xr) ->
          let Juicy.PixelRGB16 r g b = color xr
           in Juicy.PixelRGB16 (logscale k r) (logscale k g) (logscale k b)

-- | Simple (unstructured) complex number coloring.
simpleColoring :: Complex Double -> Juicy.PixelRGB16
simpleColoring z = Juicy.PixelRGB16 r g b
  where
    r = floor ((2 ^ 16 - 1) * r')
    g = floor ((2 ^ 16 - 1) * g')
    b = floor ((2 ^ 16 - 1) * b')
    (r', g', b') = hsl (phase z) 1 (1 - 0.5 ** magnitude z)

-- | Convert HSL color to RGB.
-- This function is copied with slight changes
-- from http://hackage.haskell.org/package/colour-2.3.4
hsl :: (RealFrac a, Floating a, Ord a) => a -> a -> a -> (a, a, a)
hsl h s l = (component tr, component tg, component tb)
  where
    hk = h / (2 * pi)
    (tr, tg, tb) = (mod1 (hk + 1 / 3), mod1 hk, mod1 (hk - 1 / 3))
    q
      | l < 0.5 = l * (1 + s)
      | otherwise = l + s - l * s
    p = 2 * l - q
    component t
      | t < 1 / 6 = p + ((q - p) * 6 * t)
      | t < 1 / 2 = q
      | t < 2 / 3 = p + ((q - p) * 6 * (2 / 3 - t))
      | otherwise = p
    mod1 x =
      if pf < 0
        then pf + 1
        else pf
      where
        (_, pf) = properFraction x
