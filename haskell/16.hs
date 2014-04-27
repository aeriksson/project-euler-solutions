-- 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?

import Util

euler16 = sum . toDigits

main = run $ euler16 (2^1000)
