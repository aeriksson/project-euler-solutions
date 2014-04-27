-- Take the number 192 and multiply it by each of 1, 2, and 3:
--
-- 192 x 1 = 192
-- 192 x 2 = 384
-- 192 x 3 = 576
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We
-- will call 192384576 the concatenated product of 192 and (1,2,3)
--
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
-- and 5, giving the pandigital, 918273645, which is the concatenated product of
-- 9 and (1,2,3,4,5).
--
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as
-- the concatenated product of an integer with (1,2, ... , n) where n > 1?

import Util

euler38 digs = maximum $ 0 : answers
  where dl = length digs
        bound = 10^(quot dl 2)
        candidates n = takeWhile ((<= dl) . length)
                                 (scanl1 (++) . map show $ [n, 2 * n ..])
        answers      = [read j | i <- [1..bound],
                                 j <- candidates i,
                                 isPermutation j digs]

main = run $ euler38 "123456789"
