-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?

import Util
import Data.List

nDigitPrimes n = takeWhile (< 10^n) . dropWhile (< 10^(n - 1)) $ primes

primesWithDigits digits = filter (isPermutation digits . toDigits) $ ps
  where ps = reverse . nDigitPrimes $ (length digits)

euler41 digits = head [head . primesWithDigits $ ds | ds <- tails digits,
                                                      not . divides 3 $ sum ds,
                                                      not . divides 9 $ sum ds]

main = run $ euler41 [1, 2, 3, 4, 5, 6, 7]
