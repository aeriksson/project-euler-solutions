-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain prime
-- at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
-- left: 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left
-- to right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import Util

euler37 = f [3, 5, 7] [] 10
  where f ps acc d
          | length acc == 11 = sum acc
          | otherwise        = f ps' (acc ++ acc') (10 * d)
            where ps'  = filter isPrime [p + d * i | p <- ps, i <- [1..9]]
                  acc' = filter t ps'
                  t    = all isPrime . takeWhile (> 0) . iterate (`quot` 10)

main = run euler37
