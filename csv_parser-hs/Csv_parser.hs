module Csv_parser where

import System.Environment -- getArgs
import System.IO  
import Control.Monad
import Data.List.Split -- splitOn

-- Goal:
-- Make a matrix, where each matrix row corresponds to row in csv file.

-- test data -- 
-- csv = "date,exp,type,desc\n2020-01-01,20,test2,test\n2020-01-01,20,test,test\n2020-01-06,20,test,test\n2020-02-01,20,test,test\n"

-- Description of function:
-- split contents on lines
-- remove first line (header)
-- remove empty lines
-- split each line on commas (",")
csvLines :: String -> [[String]]
csvLines csv = map (splitOn ",") $ filter (\x -> (/=) "" $ take 1 x) $ tail $ lines csv

-- parser_compiled :: IO [[String]]
-- parser_compiled = do
--  [file] <- getArgs
--  contents <- readFile file 
--  return . csvLines $ contents

parser :: FilePath -> IO [[String]]
parser path = do
 contents <- readFile path --"data.csv" -- file --"data.csv" 
 return . csvLines $ contents
