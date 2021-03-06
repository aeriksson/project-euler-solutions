-- The following iterative sequence is defined for the set of positive integers:
--
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Util

collatzLength :: Int -> Int
collatzLength n = f n 0
  where
    f 0 _ = 0
    f 1 i = i
    f n i
      | even n    = f (quot n 2) (i + 1)
      | otherwise = f (n * 3 + 1) (i + 1)

euler14 n = snd . maximum $ zip (map collatzLength [0..n]) [0..]

main = run $ euler14 1000000
