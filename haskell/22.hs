-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name
-- score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is
-- worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

import Util
import Data.List
import Data.Char

euler22 path = do contents <- readFile path
                  let names = sort . read $ "[" ++ contents ++ "]"
                  return . sum . zipWith (*) [1..] . map score $ names
  where index c = ord c - ord 'A' + 1
        score   = sum . map index

main = euler22 "../data/names.txt" >>= run
