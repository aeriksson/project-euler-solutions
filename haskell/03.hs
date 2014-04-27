-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

import Util

euler3 = maximum . primeFactors

main = run $ euler3 600851475143
