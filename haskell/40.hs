-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--
-- 0.123456789101112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If dn represents the nth digit of the fractional part, find the value of the
-- following expression.
--
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

import Util

euler40 ds = product $ map (\x -> if x < 10 then x else f x 1) ds
  where f dOffs dsPerN
          | split < dOffs = f (dOffs - split) (succ dsPerN)
          | otherwise     = toDigits targetN !! targetI
            where split   = 10^(dsPerN - 1) * 9 * dsPerN
                  targetN = 10^(dsPerN - 1) + quot dOffs dsPerN
                  targetI = pred . rem dOffs $ dsPerN

main = run $ euler40 [1, 10, 100, 1000, 10000, 100000, 1000000]
