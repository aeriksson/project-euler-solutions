-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

import Util

euler10 n = sum . takeWhile (< n) $ primes

main = run $ euler10 2000000
