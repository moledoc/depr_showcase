module Tst3 where

import Control.Parallel.Strategies

list :: [Int]
list = [1..10000000]

main :: IO ()
main = do
 let x = map (+1) list `using` parBuffer 100 rseq
 putStrLn $ show $ last $ x
