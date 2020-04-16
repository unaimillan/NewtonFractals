module Newton.Utils where

import           Data.List (find)

-- | Evaluate next 'x' approximation
-- using f and f'
nextX ::
     Fractional a
  => (a -> a) -- ^ Function f
  -> (a -> a) -- ^ Derivative f' of function f
  -> a -- ^ Current value of x
  -> a -- ^ Next value of x
nextX f f' x = x - f x / f' x

-- | Error of two adjacent iterations
approxError :: Fractional a => a -> a -> a
approxError x' x = abs (x' - x)

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
root maxN accept f f' initial = fmap snd (find (accept . fst) answers)
  where
    approximations = take maxN (iterate (nextX f f') initial)
    errors = zipWith approxError (drop 1 approximations) approximations
    answers = zip errors (zip [0 ..] approximations)
