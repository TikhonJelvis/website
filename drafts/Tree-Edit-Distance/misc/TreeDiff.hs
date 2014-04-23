module Main where

import qualified Data.Array as Array
import           Data.Array ((!))
import           Data.Tree  (Tree)

import qualified System.Environment as System

main = do [n] <- System.getArgs
          print . length . show . fib1 $ read n
          -- print . length $ take (read n) fibs1

-- Lazy memoization and dynamic programming:

-- | A simple example of lazy memoization. The fib function is
--   memoized by constructing a list of the Fibonnaci numbers lazily and
--   indexing into it.
fib :: Integer -> Integer
fib n = fibs !! fromIntegral n
  where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

fib1 :: Integer -> Integer
fib1 n = fibs1 !! fromIntegral n

fibs1 :: [Integer]
fibs1 = 0 : 1 : zipWith (+) fibs1 (drop 1 fibs1)

fib' :: Integer -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fibs ! n
  where fibs = Array.listArray (0, n) [fib' (x - 1) + fib' (x - 2) | x <- [0..n]]

fib'' :: Integer -> Integer
fib'' max = go max
  where go 0 = 0
        go 1 = 1
        go n = fibs ! n
        fibs = Array.listArray (0, max) [go (x - 1) + go (x - 2) | x <- [0..max]]

-- String Edit Distance

-- | The distance between two strings is just the number of operations
--   needed to go from one to the other. It should never be negative!
type Distance = Int 

-- | Here is the naïve string edit distance with no dynamic
--   programming. It looks very similar to the mathematical definition
--   of the problem on Wikipedia, which is nice, but also runs in
--   exponential time which is not as nice.
-- 
--   This version is particularly bad because it uses !! to access
--   list items by index. This is really slow! It would be better to
--   convert the lists to arrays, but that would also complicate the
--   code a bit.
-- 
--   For consistency, I used the same variable names as Wikipedia: two
--   strings `a' and `b' of length `n' and `m' respectively. The
--   distance between them is `d' indexed by `i' for string `b' and
--   `j' for string `a'.
naive :: Eq a => [a] -> [a] -> Distance
naive a b = d m n
  where (n, m) = (length a, length b)
        d i 0 = i
        d 0 j = j
        d i j = minimum [ d (i - 1) j + ins (b !! (i - 1))
                        , d i (j - 1) + del (a !! (j - 1))
                        , d (i - 1) (j - 1) + sub (a !! (j - 1), b !! (i - 1))
                        ]

        -- The cost functions could be arbitrary, but for now they're
        -- just constant:
        ins _ = 1
        del _ = 1
        sub (a_j, b_i)
          | a_j == b_i = 0
          | otherwise = 1
