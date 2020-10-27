module Csv_parser_v2 where

-- version, that uses Data.Text instead of String
-- import System.Environment -- getArgs

import qualified Data.Text as T --hoogle Data.Text 2020-10-10: This module is intended to be imported qualified, to avoid name clashes with Prelude functions
import qualified Data.Text.IO as T -- use strict (instead of lazy) readFile.

-- csv = T.pack "date,exp,type,desc\n2020-01-01,20,test2,test\n2020-01-01,20,test,test\n2020-01-06,20,test,test\n2020-02-01,20,test,test\n\n"

-- Goal: read in .csv file and store the file in a matrix form, where each element is a Text type object, that can be further parsed as necessary.

-- Break Text from newline characters ('\n')
-- remove header
-- remove empty lines
-- Split each line on commas.

csvLines :: T.Text -> [[T.Text]]
csvLines csv =  map  (T.splitOn $ T.pack ",") $ filter (\x -> (/=) (T.pack "") $ T.take 1 x) $ tail $ T.lines csv

-- parser_compiled :: IO [[T.Text]]
-- parser_compiled = do
--  [file] <- getArgs
--  contents <- readFile file 
--  return . csvLines $ T.pack contents

parser :: FilePath -> IO [[T.Text]]
parser path = do
 contents <- T.readFile path 
 return . csvLines $ contents

