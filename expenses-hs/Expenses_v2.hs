module Expenses_v2 where

import System.Environment -- getArgs
import System.IO  
import Control.Monad
import Data.List.Split -- splitOn

import Data.Time.Clock
import Data.Time.Calendar

import Csv_parser -- import simple csv parser

-- TODO: select 'current month - [0,1,2]'

date :: IO [Char]--(Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . yyyymm . toGregorian . utctDay
 where 
  yyyymm (a,b,_)
   | b < 10    = show a ++ "-" ++ "0" ++ show b
   | otherwise = show a ++ "-" ++ show b

-- read $ take 4 "2020-09"

