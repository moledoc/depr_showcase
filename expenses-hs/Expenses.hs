{-# LANGUAGE OverloadedStrings #-}
module Expenses where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import           Data.Csv
import           Control.Applicative
import           Data.Text (Text)
import           Data.Time (Day)


-- Parse CSV
data Expense = Expense
 { date        :: !String
 , expense     :: !Double
 , category    :: !String
 , description :: !String
 } 

instance FromNamedRecord Expense where
 parseNamedRecord r = 
  Expense 
  <$> r .: "date" 
  <*> r .: "expense" 
  <*> r .: "category" 
  <*> r .: "description"

main :: IO()
main = do
 csvData <- BL.readFile "data.csv"
 case decodeByName csvData of
  Left err -> putStrLn err
  -- TODO understand the followin code (it's lambda expression I think at the beginning)
  Right (_,v) -> V.forM_ v $ \ p ->
   putStrLn $ date p ++ "," ++ show (expense p) ++ "," ++ show (category p) ++ "," ++ show (description p)

--

-------------------------
--v2 ideas/notes
--
-- csv = readFile "data.csv"
-- test data -- csv = "date,exp,type,desc\n2020-01-01,20,test2,test\n2020-01-01,20,test,test\n2020-01-06,20,test,test\n2020-02-01,20,test,test\n"
--
-- notes
-- month == "2020-01"
-- cur_lines = filter (\x -> take 7 x == month) csv_lines
-- take 7 "2020-01-01" ==>> "2020-01"
