-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which
--
-- a^2 + b&2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

import Util

euler9 n = head [a * b * c | a <- [1..n],
                             b <- [a..n],
                             let c = n - b - a,
                             a^2 + b^2 == c^2]

main = run $ euler9 1000
