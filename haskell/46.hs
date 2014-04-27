-- It was proposed by Christian Goldbach that every odd composite number can be
-- written as the sum of a prime and twice a square.
--
-- 9 = 7 + 2×12
-- 15 = 7 + 2×22
-- 21 = 3 + 2×32
-- 25 = 7 + 2×32
-- 27 = 19 + 2×22
-- 33 = 31 + 2×12
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a
-- prime and twice a square?

import Util

euler46 n = counterexamples !! pred n
  where doubleSquares = map ((2 *) . (^2)) [1..]
        test n = none (isPrime . (n -)) . takeWhile (< n) $ doubleSquares
        counterexamples = filter test . filter odd $ composites

main = run $ euler46 1
