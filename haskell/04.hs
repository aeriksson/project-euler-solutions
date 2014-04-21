-- A palindromic number reads the same both ways. The largest palindrome made from
-- the product of two 2-digit numbers is 9009 = 91 * 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Util

is_palindrome n = n == reverse n

euler4 n = maximum $ filter (is_palindrome . show) multiples
  where lb = 10^(n - 1)
        ub = 10^n
        multiples = [i * j | i <- [lb..ub], j <- [i..ub]]

main = run $ euler4 3
