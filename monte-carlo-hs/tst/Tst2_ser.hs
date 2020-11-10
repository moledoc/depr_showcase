module Tst2_ser where


list :: [Int]
list = [1..10000000]

main :: IO ()
main = do
 let x = map (+1) list
 putStrLn $ show $ last $ x
