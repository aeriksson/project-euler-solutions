module Util where

import System.CPUTime
import Text.Printf
import Data.Char (digitToInt)
import Data.IntMap (IntMap, empty, insert, delete, member, (!))
import Data.List (group, sort, tails)
import Data.Set (fromList, toList)

distinct :: Ord a => [a] -> [a]
distinct = toList . fromList

none :: (a -> Bool) -> [a] -> Bool
none f = not . any f

sublistsOfLength :: Int -> [a] -> [[a]]
sublistsOfLength n l = map (take n) . filter ((>= n) . length) . tails $ l

primes :: [Int]
primes = 2 : 3 : 5 : 7 : sieve 9 (tail primes) empty
  where
    sieve_add :: IntMap Int -> Int -> Int -> IntMap Int
    sieve_add sv start step
      | member next sv = sieve_add sv next step
      | otherwise      = insert next step sv
        where next = start + step

    sieve :: Int -> [Int] -> IntMap Int -> [Int]
    sieve c ps@ ~(p:pt) sv
      | member c sv =     sieve c' ps (sieve_add (delete c sv) c (sv ! c))
      | c >= p * p  =     sieve c' pt (sieve_add sv c (2 * p))
      | otherwise   = c : sieve c' ps sv
        where c' = c + 2

composites :: [Int]
composites = concatMap nsBetween $ zip primes (tail primes)
  where nsBetween (a, b) = [succ a .. pred b]

coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1

divides :: Integral a => a -> a -> Bool
divides a b = mod b a == 0

isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\x -> not $ divides x n) $ takeWhile (<= rt) primes
    where rt = floor $ sqrt $ fromIntegral n

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

properDivisors :: Int -> [Int]
properDivisors n = filter (`divides` n) [1..(pred n)]

properDivisorSum :: Int -> Int
properDivisorSum n
  | n < 2     = 0
  | otherwise = f partitionedPrimes 1 - n
  where f []        s = s
        f ps@(p:pt) s = f pt (s * multiplier)
          where pows       = iterate (* head p) (head p)
                multiplier = succ . sum . take (length p) $ pows
        partitionedPrimes = group . primeFactors $ n

primeFactors :: Int -> [Int]
primeFactors = factors primes
  where
    factors :: [Int] -> Int -> [Int]
    factors ps@(p:pt) n
      | n < 2        = []
      | rem n p == 0 = p : factors ps (quot n p)
      | p * p > n    = [n]
      | otherwise    = factors pt n

fibonacciNumbers :: [Integer]
fibonacciNumbers = 0 : 1 : zipWith (+) fibonacciNumbers
                                        (tail fibonacciNumbers)

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation a b = sort a == sort b

factorial :: Integral a => a -> a
factorial n = product [1..n]

choose :: Integral a => a -> a -> a
choose n k = quot (product [n + 1 - i | i <- [1..k]]) (product [1..k])

nDivisors :: Int -> Int
nDivisors n = product $ map (\x -> 1 + length x) $ (group . primeFactors) n

toDigitsInBase :: Integral a => a -> a -> [Int]
toDigitsInBase b n = f n []
  where f 0 acc = acc
        f n acc = f (quot n b) (fromIntegral (rem n b) : acc)

toDigits :: Integral a => a -> [Int]
toDigits = toDigitsInBase 10

fromDigits :: (Integral a, Show a, Integral b, Read b) => [a] -> b
fromDigits = read . concatMap show

maxIndex :: Ord a => [a] -> Int
maxIndex l = snd $ maximum $ zip l [0..]

run :: Show a => a -> IO ()
run f = do
    start <- getCPUTime
    return $! f
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10^9)
    printf "Elapsed time: %0.3f msecs\n" (diff :: Double)
    print f
