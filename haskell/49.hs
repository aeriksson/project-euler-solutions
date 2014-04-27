-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms are
-- prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?

import Util

triples n = [(a, b, c) | b <- ps,
                         a <- takeWhile (< b) ps,
                         let c = b + (b - a),
                         isPermutation (show a) (show b),
                         isPermutation (show a) (show c),
                         isPrime c]
  where ps = takeWhile (< 10 ^ n) . dropWhile (< 10 ^ pred n) $ primes

euler49 = toRes . head . filter (/= (1487, 4817, 8147)) . triples
  where toRes (a, b, c) = fromDigits $ toDigits a ++ toDigits b ++ toDigits c

main = run $ euler49 4
