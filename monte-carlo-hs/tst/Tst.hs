module Tst where

import Control.Parallel.Strategies

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
 let (a,b) = runEval $ do {
   ; a <- rpar $ fib 38
   ; b <- rpar $ fib 37
   ; rseq a
   ; rseq b
   ; return (a,b)
 }
 putStrLn $ show (a,b)
