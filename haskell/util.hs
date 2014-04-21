module Util where

import System.CPUTime
import Text.Printf
import Data.Char (digitToInt)
import Data.IntMap hiding (map)
import Data.List (group)

primes :: [Int]
primes = 2 : 3 : 5 : 7 : sieve 9 (drop 1 primes) empty
  where
    sieve_add :: IntMap Int -> Int -> Int -> IntMap Int
    sieve_add sv start step
      | notMember next sv = insert next step sv
      | otherwise         = sieve_add sv next step
        where next = start + step

    sieve :: Int -> [Int] -> IntMap Int -> [Int]
    sieve c ps@ ~(p:pt) sv
      | member c sv =     sieve c' ps (sieve_add (delete c sv) c (sv ! c))
      | c >= p * p  =     sieve c' pt (sieve_add sv c (2 * p))
      | otherwise   = c : sieve c' ps sv
        where c' = c + 2

primeFactors :: Int -> [Int]
primeFactors n = factors primes n
  where
    factors :: [Int] -> Int -> [Int]
    factors _ 1 = []
    factors ps@(p:pt) n
      | rem n p == 0 = p : factors ps (quot n p)
      | p * p > n    = [n]
      | otherwise    = factors pt n

fibonacciNumbers :: [Integer]
fibonacciNumbers = 0 : 1 : zipWith (+) fibonacciNumbers
                                        (tail fibonacciNumbers)

factorial :: Integral a => a -> a
factorial n = product [1..n]

choose :: Integral a => a -> a -> a
choose n k = quot (product [n + 1 - i | i <- [1..k]]) (product [1..k])

nDivisors :: Int -> Int
nDivisors n = product $ map (\x -> 1 + length x) $ (group . primeFactors) n

digitsOf :: Integer -> [Int]
digitsOf = map digitToInt . show

run :: t -> IO t
run a = do
    start <- getCPUTime
    v <- return $! a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^9)
    printf "Elapsed time: %0.3f msecs\n" (diff :: Double)
    return v
