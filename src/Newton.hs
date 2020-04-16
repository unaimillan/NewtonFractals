module Newton where

-- | Find a root of an equation
-- f(x) = 0
-- using Newton's method.
root ::
     Fractional a
  => Int -- ^ Maximum number of iterations.
  -> (a -> Bool) -- ^ An acceptable error (precision).
  -> (a -> a) -- ^ Function f to find zero for.
  -> (a -> a) -- ^ Derivative f' of function f.
  -> a -- ^ Initial guess.
  -> Maybe (Int, a) -- ^ Number of iterations and root
                    -- (where function is zero), if found.
root = undefined

main :: IO ()
main = print "Hello world!!"
