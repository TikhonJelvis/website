module Main where

import qualified Data.Array as Array
import           Data.Array ((!))
import           Data.Tree  (Tree)

import qualified System.Environment as System

import           Text.Printf (printf)

main = do [f₁, f₂, flag] <- System.getArgs
          a <- readFile f₁
          b <- readFile f₂
          if flag == "basic"
            then printf "Basic result: %d\n" $ basic a b
            else printf "Better result: %d\n" $ better a b

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
fib' max = go max
  where go 0 = 0
        go 1 = 1
        go n = fibs ! (n - 1) + fibs ! (n - 2)
        fibs = Array.listArray (0, max) [go x | x <- [0..max]]

-- | This version is absolutely broken since fibs gets created a new
--   at each recursive call!
fib'' :: Integer -> Integer
fib'' 0 = 0
fib'' 1 = 1
fib'' n = fibs ! (n - 1) + fibs ! (n - 2)
  where fibs = Array.listArray (0, n) [fib'' x | x <- [0..n]]

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
--   For consistency, I used similar variable names to Wikipedia: two
--   strings `a' and `b' of length `m' and `n' respectively. The
--   distance between them is `d' indexed by `i' for string `a' and
--   `j' for string `b'.
naive :: Eq a => [a] -> [a] -> Distance
naive a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = d (i - 1) (j - 1)
          | otherwise = minimum [ d (i - 1) j       + 1
                                , d i (j - 1)       + 1
                                , d (i - 1) (j - 1) + 1
                                ]

-- | Here is the basic algorithm with dynamic programming. It does
--   this by circularly defining a lazy array: the function replaces
--   recursion by indexing into the array, and each array item just
--   calls back into the function.
--
--   It still uses !! to index on lists though, which is a major
--   problem for even moderately sized strings.
basic :: Eq a => [a] -> [a] -> Distance
basic a b = d m n
  where (m, n) = (length a, length b)
        d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = Array.listArray bounds [d i j | (i, j) <- Array.range bounds]
        bounds = ((0, 0), (m, n))

-- | This version is just like @basic@ except that it writes the lists
--   into arrays and indexes on those, which is a considerable
--   improvement in practice. It's still not super-fast in absolute
--   terms, but I think it's pretty good given how elegant the code
--   is.
better :: Eq a => [a] -> [a] -> Distance
better a b = d m n
  where (m, n) = (length a, length b)
        a'     = Array.listArray (1, m) a
        b'     = Array.listArray (1, n) b

        d i 0 = i
        d 0 j = j
        d i j
          | a' ! i ==  b' ! j = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = Array.listArray bounds [d i j | (i, j) <- Array.range bounds]
        bounds = ((0, 0), (m, n))
