-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

import Util

euler5 = foldr1 lcm

main = run $ euler5 [1..20]
