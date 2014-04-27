-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:
--
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:
--
-- 1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
-- How many different ways can £2 be made using any number of coins?

import Util
import Data.Sequence hiding (replicate, null)

euler31 coins n = f vs (head coins) coins
  where
    vs = fromList $ 1 : replicate n 0
    f v i cs@(c:ct)
      | i < n     = f v' (succ i) cs
      | null ct   = ways
      | otherwise = f v' (head ct) ct
      where ways = index v i + index v (i - c)
            v'   = update i ways v

main = run $ euler31 coins 200
  where coins = [1, 2, 5, 10, 20, 50, 100, 200]
