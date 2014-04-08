-- A palindromic number reads the same both ways. The largest palindrome made from
-- the product of two 2-digit numbers is 9009 = 91 * 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Util

is_palindrome x = x == reverse x

main = do
  run $ maximum $ filter (is_palindrome . show) [i * j | i <- [100..1000], j <- [i..1000]]
