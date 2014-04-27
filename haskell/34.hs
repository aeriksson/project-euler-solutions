-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Util
import Data.Vector (fromList, (!))

euler34 = sum $ filter (\n -> n == facSum n) [2 .. 7 * factorial 9]
  where facs   = fromList . map factorial $ [0 .. 9]
        facSum = sum . map (facs !) . toDigits

main = run euler34
