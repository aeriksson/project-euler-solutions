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

spellOut n
  | n == 0    = ""
  | n < 20    = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                 "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
                 "seventeen", "eighteen", "nineteen"] !! (n - 1)
  | n < 100   = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
                 "ninety"] !! (div n 10 - 2) ++ spellOut (mod n 10)
  | n < 1000  = spellOut (div n 100)  ++ "hundred"  ++ spellOut (mod n 100)
  | n < 10000 = spellOut (div n 1000) ++ "thousand" ++ spellOut (mod n 1000)
  | otherwise = ""

euler17 l = (+ 3 * nAnds l) . sum . map (length . spellOut) $ l
  where nAnds = length . filter (\n -> n > 100 && not (divides 100 n))

main = run $ euler17 [1..1000]
