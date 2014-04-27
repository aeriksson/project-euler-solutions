-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up
-- of each of the digits 0 to 9 in some order, but it also has a rather
-- interesting sub-string divisibility property.
--
-- Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
-- the following:
--
-- d2d3d4=406 is divisible by 2
-- d3d4d5=063 is divisible by 3
-- d4d5d6=635 is divisible by 5
-- d5d6d7=357 is divisible by 7
-- d6d7d8=572 is divisible by 11
-- d7d8d9=728 is divisible by 13
-- d8d9d10=289 is divisible by 17
--
-- Find the sum of all 0 to 9 pandigital numbers with this property.

import Util
import Data.List

subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

validPrefixes divisor suffix digits = nub . filter validPre $ digits \\ suffix
  where
    validPre = divides divisor . fromInteger . fromDigits . take 3 . (: suffix)

numsWithProperty digits suffix [] =
  nub . map (fromDigits . (++ suffix)) $ permutations digits
numsWithProperty digits [] (divisor : divisors) =
  concat [numsWithProperty digits' p divisors | digs <- filter ((== 3) . length) . subsets $ digits,
                                 p <- permutations digs,
                                 divides divisor . fromDigits $ p,
                                 let digits' = digits \\ p]
numsWithProperty digits suffix (divisor : divisors) =
  concat [numsWithProperty digits' suffix' divisors | d <- validPrefixes divisor suffix digits,
                                       let digits' = delete d digits,
                                       let suffix' = d : suffix]

euler43 digits divisors = sum $ numsWithProperty digits [] (reverse divisors)

main = run $ euler43 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] [2, 3, 5, 7, 11, 13, 17]
