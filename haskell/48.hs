-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

import Util

euler48 n m = lastDigits m . sum . map (\x -> x^x) $ [1..n]
  where lastDigits m = fromDigits . reverse . take m . reverse . toDigits

main = run $ euler48 1000 10
