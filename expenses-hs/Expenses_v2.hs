module Expenses_v2 where

import System.Environment -- getArgs
import System.IO  
import Control.Monad
import Data.List.Split -- splitOn

import Data.Time.Clock
import Data.Time.Calendar

-- Make the csv output in matrix form (each row in csv is row in matrix)
csvLines :: String -> [[String]]
csvLines csv = map (splitOn ",") $ filter (\x -> (/=) "" $ take 1 x) $ tail $ lines csv

-- TODO: select 'current month - [0,1,2]'

date :: IO [Char]--(Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . yyyymm . toGregorian . utctDay
 where 
  yyyymm (a,b,_)
   | b < 10    = show a ++ "-" ++ "0" ++ show b
   | otherwise = show a ++ "-" ++ show b

-- read $ take 4 "2020-09"

main :: IO ()
main = do
 -- [file] <- getArgs
 contents <- readFile "data.csv" --file --"data.csv"
 print . csvLines $ contents
