-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

import Util

spell_out n
  | n == 0    = ""
  | n < 20    = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                 "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
                 "seventeen", "eighteen", "nineteen"] !! (n - 1)
  | n < 100   = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
                 "ninety"] !! (div n 10 - 2) ++ spell_out (mod n 10)
  | n < 1000  = spell_out (div n 100) ++ "hundred" ++ spell_out (mod n 100)
  | n < 10000 = spell_out (div n 1000) ++ "thousand" ++ spell_out (mod n 1000)
  | otherwise = ""

main = do run $ sum $ map (length . spell_out) [1..1000]
