-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.

import Util

euler32 digits = sum . distinct $ [a * b | (as, bs) <- bounds $ length digits,
                                           a <- as,
                                           b <- bs a,
                                           let p  = a * b
                                               d = concatMap toDigits [a, b, p],
                                           isPermutation digits d]
  where bounds n = case n of 10 -> [([10..99], (\x -> [quot 10000 x .. 1000])),
                                    ([1..9],   (\x -> [quot 10000 x .. 10000]))]
                             9  -> [([10..99], (\x -> [100  .. quot 10000 x])),
                                    ([1..9],   (\x -> [1000 .. quot 10000 x]))]
                             8  -> [([10..99], (\x -> [quot 1000 x .. 100])),
                                    ([1..9],   (\x -> [quot 1000 x .. 1000]))]
                             7  -> [([10..99], (\x -> [10  .. quot 1000 x])),
                                    ([1..9],   (\x -> [100 .. quot 1000 x]))]
                             6  -> [([10..99], (\x -> [quot 10000 x .. 1000]))]
                             5  -> [([10..99], (\x -> [100  .. quot 10000 x]))]
                             4  -> [([10..99], (\x -> [quot 1000 x .. 100]))]
                             3  -> [([10..99], (\x -> [10  .. quot 1000 x]))]
                             otherwise -> []

main = run $ euler32 [1, 2, 3, 4, 5, 6, 7, 8, 9]
