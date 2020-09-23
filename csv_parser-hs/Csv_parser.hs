module Csv_parser where

import Data.List.Split
-- splitOn

-- Goal:
-- Make a matrix, where each matrix row corresponds to row in csv file.

-- test data -- csv = "date,exp,type,desc\n2020-01-01,20,test2,test\n2020-01-01,20,test,test\n2020-01-06,20,test,test\n2020-02-01,20,test,test\n"

csvLines :: String -> [[String]]
csvLines csv = map (splitOn ",") $ filter (\x -> (/=) "" $ take 1 x) $ tail $ splitOn "\n" csv


