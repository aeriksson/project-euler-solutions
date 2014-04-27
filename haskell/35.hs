-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
-- 73, 79, and 97.
--
-- How many circular primes are there below one million?

import Util
import Data.List (groupBy)
import Data.Set

euler35 n = length $ [c | (i, ps) <- zip [0..] primeSets,
                          c       <- toList $ (iterate cycled ps) !! i]
  where candidates  = takeWhile (< n) primes
        primeSets   = Prelude.map fromList $ groupBy sameLen $ candidates
        sameLen x y = length (toDigits x) == length (toDigits y)
        cycled s    = intersection s $ Data.Set.map cycleInt s
        cycleInt n  = let d = pred . length . toDigits $ n
                        in quot n 10 + 10^d * rem n 10

main = run $ euler35 (10^6)
