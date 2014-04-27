-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
-- so the first ten triangle numbers are:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- By converting each letter in a word to a number corresponding to its
-- alphabetical position and adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
-- is a triangle number then we shall call the word a triangle word.
--
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
-- containing nearly two-thousand common English words, how many are triangle
-- words?

import Util
import Data.Char
import Data.List

import Util

isTriangular = isInt . sqrt . fromIntegral . succ . (* 8)

euler42 path = do contents <- readFile path
                  let scores = map score . sort . read $ "[" ++ contents ++ "]"
                  return . length . filter isTriangular $ scores
  where index c = ord c - ord 'A' + 1
        score   = sum . map index

main = euler42 "../data/words.txt" >>= run
