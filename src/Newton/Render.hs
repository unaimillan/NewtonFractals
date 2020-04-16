module Newton.Render where

-- this module comes from JuicyPixels package
import qualified Codec.Picture            as J
import           Codec.Picture.ColorQuant (defaultPaletteOptions)
import           Data.Complex
import           Newton.Utils             (root)

-- | Add brightness to an RGB8 Image
brightnessRGB8 :: Int -> J.Palette -> J.Palette
brightnessRGB8 add = J.pixelMap brightFunction
  where
    up v = fromIntegral (fromIntegral v + add)
    brightFunction (J.PixelRGB8 r g b) = J.PixelRGB8 (up r) (up g) (up b)

-- | Save an PNG image as a file
saveBasinsPic ::
     FilePath -- ^ Where to write the image file.
  -> J.Palette
  -> IO ()
saveBasinsPic = J.writePng

saveBasinsAnim ::
     FilePath -- ^ Filename of an image
  -> J.GifDelay -- ^ Delay between frames
  -> [J.Palette] -- ^ List of pictures to store within the GIF
  -> IO ()
saveBasinsAnim path delay images =
  case J.writeGifImages path J.LoopingForever finalImgs of
    Left msg  -> error msg
    Right ans -> ans
  where
    palettized = map (J.palettize defaultPaletteOptions) images
    finalImgs = map (\(img, plt) -> (plt, delay, img)) palettized

-- | Render attraction basins for zeros (roots) of a complex function
-- as a PNG image.
renderBasins ::
     (Fractional a, RealFloat a)
  => Int -- ^ Size of the square image in pixels
  -> (Complex a -> Complex a) -- ^ A function f to find zero for.
  -> (Complex a -> Complex a) -- ^ A derivative of function f.
  -> (Complex a -> J.PixelRGB8) -- ^ How to color roots.
  -> J.Palette
renderBasins commonSize f f' color = image
  where
    image = J.generateImage render width height
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
    render :: Int -> Int -> J.PixelRGB8
    render i j =
      case root n p f f' (pixelToComplex (i, j)) of
        Nothing -> J.PixelRGB8 0 0 0 -- black
        Just (k, xr) ->
          let J.PixelRGB8 r g b = color xr
           in J.PixelRGB8 (logscale k r) (logscale k g) (logscale k b)

-- | Simple (unstructured) complex number coloring.
simpleColoring :: Complex Double -> J.PixelRGB8
simpleColoring z = J.PixelRGB8 r g b
  where
    r = floor ((2 ^ 8 - 1) * r')
    g = floor ((2 ^ 8 - 1) * g')
    b = floor ((2 ^ 8 - 1) * b')
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
