module Expenses_v3 where

-- TODOs
-- comment program
-- refactor maybe

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

lower_Bound :: Expense
lower_Bound = E (T.pack "2020-01-01") 0.0 (T.pack "lower_bound") (T.pack "lower_bound")
upper_Bound :: Expense
upper_Bound = E (T.pack "2020-01-01") 0.0 (T.pack "upper_bound") (T.pack "upper_bound")

-- Define ordering by date only.
instance Ord Expense where
 -- compare E (date _ _ _) (E date2 _ _ _) = if date == date2 then EQ
 -- compare E {date = d1} E{date = d2} = if d1 == d2 then EQ
 --  else if d1 <= d2 then LT
 --  else GT
 (<=) E {date = d1} E{date = d2} = (<=) d1 d2

text2int :: [T.Text] -> Int
text2int xs = read ( T.unpack ( xs !! 1 )) :: Int

text2double :: [T.Text] -> Double
text2double xs = read ( T.unpack ( xs !! 1 )) :: Double

parse_le10 :: Int -> String
parse_le10 val
 | val < 10 = "0" ++ show val
 | otherwise = show val

-- make list of Expenses
-- expenses :: IO ([Expense])
expenses :: IO ()
expenses = do
 content <- parser "data.csv"
 let expenses_done = map make_expense content
 print_help
 loop
 -- expenses'
 -- return expenses_done
  where 

   make_expense :: [T.Text] -> Expense
   make_expense xs = E {
      date        = xs !! 0
    , expense     = text2double xs
    , category    = xs !! 2
    , description = xs !! 3
   }

   get_input :: String -> IO (String)
   get_input prompt = do
    putStrLn prompt
    input <- getLine
    return input

   print_help :: IO ()
   print_help = putStrLn "-- Help -- \n h - show this help \n a - add new expense \n r - report expenses from given \n q - exit program "
   
   add_exp :: IO ()
   add_exp = do
    date        <- get_input "Date in yyyy-mm-dd format:"
    expense     <- get_input "Expense amount:"
    category    <- get_input "Category of expense:"
    description <- get_input "Description of expense:"
    commit      <- get_input "Commit new expense [y/n]?"
    let new_exp = make_expense $ map T.pack $ [date,expense,category,description]
    if commit == "y" then do
     putStrLn "TODO"
    else
     putStrLn "Expense not commited: " >> print new_exp

   report_exp = do
    -- lower_bound <- get_input
    lwr_bnd <- get_input "Select month to report in format yyyy-mm: "
    -- TODO: check user input
    let lower_bound =  T.pack $ lwr_bnd ++ "-01"
    let upr_bnd_month = parse_le10 $ (+1) $ text2int $ T.splitOn (T.pack "-") $ T.pack lwr_bnd
    let upr_bnd_year = T.unpack $ (!! 0) $ T.splitOn (T.pack "-") $ T.pack lwr_bnd
    let upper_bound = T.pack $ (++) upr_bnd_year $ (++) "-" $ (++) upr_bnd_month "-00"
    -- TMP
    content <- parser "data.csv"
    let expenses_done = map make_expense content
    -- TODO MAKE NICER
    let tst =  filter (< upper_Bound {date = upper_bound}) $ filter (>= lower_Bound {date = lower_bound}) expenses_done
    print tst
    -- (filter (>= E {date = lower_bound}))
    -- print lower_bound
    -- print upper_bound

   parse_cmd :: String -> IO ()
   parse_cmd cmd
    | cmd == "q" = putStrLn "Closing the program"
    | cmd == "h" = print_help                  >> loop
    | cmd == "a" = add_exp                     >> loop
    | cmd == "r" = report_exp                  >> loop
    | otherwise  = putStrLn "Unknown command!" >> loop

   loop :: IO ()
   loop = do
    cmd <- get_input "Insert command: "
    parse_cmd cmd

-- control = do
--  exp_list <- expenses
--  -- mapM_ print exp_list
--  let tst = (exp_list !! 0) <= (exp_list !! 1)
--  print tst
