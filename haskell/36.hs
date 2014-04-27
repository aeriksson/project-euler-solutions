-- The decimal number, 585 = 1001001001b (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

import Util

euler36 n = sum . filter (palInBase 2) . filter (palInBase 10) $ [1, 3 .. n]
  where palInBase b n = let m = toDigitsInBase b n in m == reverse m

main = run $ euler36 (10^6)
