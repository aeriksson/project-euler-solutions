-- 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 21000?

import Util

euler16 n = sum $ digitsOf n

main = run $ euler16 (2^100)
