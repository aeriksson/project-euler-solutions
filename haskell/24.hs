-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order.
-- The lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?

import Util
import Data.List
import Data.Char

nthPermutation xs 1 = xs
nthPermutation xs i = x : nthPermutation (delete x xs) (rem i n)
      where n = factorial (length xs - 1)
            x = xs !! quot i n

euler24 :: [Int] -> Int -> Int
euler24 xs n = read $ map intToDigit $ nthPermutation xs n

main = run $ euler24 [0..9] (10^6 - 1)
