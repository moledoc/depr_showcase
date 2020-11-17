module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz x
 | (mod) x 3 == 0 && (mod) x 5 == 0 = "FizzBuzz"
 | (mod) x 3 == 0                   = "Fizz"
 | (mod) x 5 == 0                   = "Buzz"
 | otherwise                        = show x




fizz :: Int -> String
fizz x
 | (mod) x 3 == 0 = "Fizz"
 | otherwise = ""

buzz :: Int -> String
buzz x
 | (mod) x 5 == 0 = "Buzz"
 | otherwise = ""

fizzbuzzV2 :: Int -> String -> String -> String
fizzbuzzV2 x "" "" = show x
fizzbuzzV2 _ fizz buzz = fizz ++ buzz

fb :: Int -> Int -> String -> String
fb x n str
 | (mod) x n == 0 = str
 | otherwise      = ""

main :: IO ()
main = do
 -- _ <- mapM (\x -> putStrLn $ fizzbuzz x) [1..100]
 -- _ <- mapM (\x -> putStrLn $ fizzbuzzV2 x (fizz x) (buzz x)) [1..100]
 _ <- mapM (\x -> putStrLn $ fizzbuzzV2 x (fb x 3 "Fizz") (fb x 5 "Buzz")) [1..100]
 putStrLn ""