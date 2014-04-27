-- The prime 41, can be written as the sum of six consecutive primes:
--
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below
-- one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a
-- prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most
-- consecutive primes?

import Util

euler50 n = head [d | i <- [0..nSums],
                      d <- map diff . partition (nSums - i) $ sums,
                      isPrime d]
  where sums  = takeWhile (< n) . scanl1 (+) $ primes
        nSums = length sums
        diff l = last l - head l

main = run $ euler50 (10^6)
