module Expenses_v3 where

import qualified Data.Text as T
import Control.Monad -- forM_
import Csv_parser_v2 -- self written simple parser

-- Define new data type Expense
data Expense = E {
   date        :: T.Text
 , expense     :: Double
 , category    :: T.Text
 , description :: T.Text
} deriving (Show,Eq)

-- Define ordering by date only.
instance Ord Expense where
 -- compare E (date _ _ _) (E date2 _ _ _) = if date == date2 then EQ
 compare E {date = d1} E{date = d2} = if d1 == d2 then EQ
  else if d1 <= d2 then LT
  else GT


-- make list of Expenses
expenses :: IO ([Expense])
expenses = do
 content <- parser "data.csv"
 let expenses_done = map make_expense content
 return expenses_done
  where make_expense xs = E {
     date        = xs !! 0
   , expense     = read ( T.unpack ( xs !! 1 )) :: Double -- TODO: refactor this
   , category    = xs !! 2
   , description = xs !! 3
  }

control = do
 exp_list <- expenses
 -- mapM_ print exp_list
 let tst = (exp_list !! 0) <= (exp_list !! 1)
 print tst
