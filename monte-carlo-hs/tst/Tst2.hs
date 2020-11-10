module Tst2 where

import Control.Parallel.Strategies

list :: [Int]
list = [1..10000000]

main :: IO ()
main = do
 let x = parMap rseq (+1) list
 putStrLn $ show $ last $ x
