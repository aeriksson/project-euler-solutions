-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120.
--
-- {20,48,52}, {24,45,51}, {30,40,50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?

import Util
import Data.List

triplesWithPerimiter p = distinct $ newTriples ++ oldTriples
  where
    lowerBound = succ $ floor $ sqrt $ (fromIntegral p) / 4
    upperBound = pred $ ceiling $ sqrt $ (fromIntegral p) / 2
    candidates = filter (\x -> divides (2 * x) p) [lowerBound .. upperBound]
    newTriples = map (sort . euclidTr) candidates
    oldTriples = [map (\x -> x * (quot p d)) ts | d <- properDivisors p,
                                                  ts <- triplesWithPerimiter d]
    euclidTr m = let n = quot p (2 * m) - m in [m^2 - n^2, 2 * m * n, m^2 + n^2]

euler39 n = maxIndex $ map (length . triplesWithPerimiter) [0..(pred n)]

main = run $ euler39 1000
