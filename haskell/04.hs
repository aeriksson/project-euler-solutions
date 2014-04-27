-- A palindromic number reads the same both ways. The largest palindrome made from
-- the product of two 2-digit numbers is 9009 = 91 * 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Util

isPalindrome n = n == reverse n

euler4 n = maximum $ filter (isPalindrome . show) multiples
  where upper = 10^n
        lower = 10^(n - 1)
        multiples = [i * j | i <- [lower .. upper], j <- [i .. upper]]

main = run $ euler4 3
