-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
--
-- 1/2  = 0.5
-- 1/3  = 0.(3)
-- 1/4  = 0.25
-- 1/5  = 0.2
-- 1/6  = 0.1(6)
-- 1/7  = 0.(142857)
-- 1/8  = 0.125
-- 1/9  = 0.(1)
-- 1/10 = 0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.

import Util

mulOrder x n = head [if j == 1 then i else 0 | (i, j) <- pows x n, j < 2]
  where pows x n = zip [1..] (iterate (\y -> mod (x * y) n) x)

decimalCycleLength denom = mulOrder 10 coprimeToTen
  where coprimeToTen = rmMults 5 $ rmMults 2 $ denom
        rmMults n p
          | (mod p n == 0) = rmMults (quot p n) n
          | otherwise      = p

euler26 n = snd . maximum $ zip (map decimalCycleLength [2..999]) [2..]

main = run $ euler26 1000
