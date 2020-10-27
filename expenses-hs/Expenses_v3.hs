module Expenses_v3 where

----------
-- imports
import qualified Data.Text as T
import Text.Read -- readMaybe
import Control.Monad -- forM_
import Control.Monad.State as MS -- State
import Csv_parser_v2 -- self written simple parser

--------------------
-- global variables
dataFile :: String
dataFile = "data.csv"

default_expense :: Expense
default_expense = E (T.pack "1900-01") 0.0 (T.pack "default") (T.pack "default")

-- Default lower and upper bounds, that are used to create new bounds when reporting.
lower_Bound :: Expense
lower_Bound = E (T.pack "2020-01-01") 0.0 (T.pack "lower_bound") (T.pack "lower_bound")
upper_Bound :: Expense
upper_Bound = E (T.pack "2020-01-01") 0.0 (T.pack "upper_bound") (T.pack "upper_bound")

----------------------
-- Defined data type
-- and it's instances

data Expense = E {
   date        :: T.Text
 , expense     :: Double
 , category    :: T.Text
 , description :: T.Text
} deriving (Show,Eq)

-- Define ordering by date only.
instance Ord Expense where
 (<=) E {date = d1} E{date = d2} = (<=) d1 d2

instance Num Expense where
 (+) E {expense = e1,date = d1} E{expense = e2} = default_expense {expense = e1 + e2}
 (*) E {expense = e1} E{expense = e2} = default_expense {expense = e1 * e2}
 (-) E {expense = e1} E{expense = e2} = default_expense {expense = e1 - e2}
 -- abs x = x
 -- signum x = x
 fromInteger n = default_expense {expense = (fromInteger n) :: Double}
 abs E {expense = e1} = if e1 < 0 then default_expense {expense = (-e1)}
  else default_expense {expense = e1}
 signum E {expense = e1} = case compare e1 0 of
  GT -> default_expense {expense = 1}
  LT -> default_expense {expense = (-1)}
  EQ -> default_expense {expense = 0}
 
-------
-- MAIN
main :: IO String
main = do
 content <- parser dataFile
 let state = map make_expense content
 loop print_help state

--HELPER functions

-- To make an expense from a list of Text
make_expense :: [T.Text] -> Expense
make_expense xs = E {
   date        = xs !! 0
 , expense     = text2double (xs !! 1)
 , category    = xs !! 2
 , description = xs !! 3
 }

-- Make T.Text to Double
text2double :: T.Text -> Double
text2double x = read ( T.unpack x ) :: Double

--------------------------------------------------

-------
-- loop

loop :: String -> [Expense] -> IO String
loop prompt state = do
 putStrLn prompt
 cmd <- get_input "Insert command: "
 parse_cmd cmd state

-- Given user input, do action.
parse_cmd :: String -> [Expense] -> IO String
parse_cmd cmd state
 | cmd == "q" = return "Closing the program"
 | cmd == "h" = loop print_help state
 | cmd == "a" = add_exp    state >>= (\(prompt,state) -> loop prompt state)
 | cmd == "r" = report_exp state >>= (\prompt -> loop prompt state)
 | otherwise  = loop "Unknown command!" state

-- HELPER functions

-- Get user input using given prompt.
get_input :: String -> IO (String)
get_input prompt = do
 putStrLn prompt
 input <- getLine
 return input

--------------------------------------------------

---------------
-- ACTION: help

print_help :: String
print_help = "-- Help -- \n h - show this help \n a - add new expense \n r - report expenses from given \n q - exit program"

--------------
-- ACTION: add

add_exp :: [Expense] -> IO (String, [Expense])
add_exp state = do
  date        <- validDate "Date in yyyy-mm-dd format:" 3
  expense     <- validAmount "Expense amount:"
  category    <- get_input "Category of expense:"
  description <- get_input "Description of expense:"
  commit      <- get_input "Commit new expense [y/n]?"
  let new_exp_el = [date,expense,category,description]
  let new_exp = make_expense $ map T.pack new_exp_el
  if commit == "y" then do
   appendFile dataFile (foldr addComas [] new_exp_el)
   print new_exp
   return ("Expense commited",new_exp : state)
  else do
   print new_exp
   return ("Expense not commited",state)
  where 
   addComas x [] = x ++ "\n"
   addComas x y  = x ++ "," ++ y

-- HELPER functions

---- validAmount
-- get amount input from user.
validAmount :: String -> IO String
validAmount prompt = do
 amount <- get_input prompt
 let amount_d = readMaybe amount :: Maybe Double
 if amount_d /= Nothing then
   return amount
 else do
  putStrLn "Invalid number"
  validAmount prompt

----- validDate
-- get date value from user
validDate :: String -> Int -> IO String
validDate prompt n = do
 date <- get_input prompt
 let parts = map T.unpack $ T.splitOn (T.pack "-") $ T.pack date
 let len_parts = length parts
 -- if the number of elements is wrong, then ask the date again.
 if len_parts > n || len_parts < n then do
  putStrLn "Invalid date"
  validDate prompt n
 else do
  -- if checkDate is not correct, ask date again.
  if (checkDate parts n) /= date then do
   putStrLn "Invalid date"
   validDate prompt n
  else
   return date

-- check validity of given date
-- either for format yyyy-mm or yyyy-mm-dd.
checkDate :: [String] -> Int -> String
checkDate parts 2 = (checkYear (parts !! 0)) ++ "-" ++ (checkMonth (parts !! 1))
checkDate parts 3 = (checkYear (parts !! 0)) ++ "-" ++ (checkMonth (parts !! 1)) ++ "-" ++ (checkDay (parts !! 2) (parts !! 1) (parts !! 0))

-- Check, if given year is valid and in wanted format.
checkYear :: String -> String
checkYear x = g (readMaybe x :: Maybe Int)
 where 
  g Nothing  = "0"
  g (Just _) = x

-- Check, if given month is valid and in wanted format.
checkMonth :: String -> String
checkMonth x = g (readMaybe x :: Maybe Int)
 where 
  g Nothing  = "0"
  g (Just y)
   | y <= 12 && y>0  = x
   | otherwise       = "0"

-- Check if given day is a valid day given month (and leap/non-leap year)
-- and in wanted format.
checkDay :: String -> String -> String -> String
checkDay day month year = g (readMaybe day :: Maybe Int)
 where
  g Nothing = "0"
  g (Just y)
   | (elem) month ["01","03","05","07","08","10","12"] && y <= 31      = day
   | (elem) month ["04","06","09","11"] && y <= 30                     = day
   | (elem) month ["02"] && y <= 29 && (mod) (read year :: Int) 4 == 0 = day
   | (elem) month ["02"] && y < 29  && (mod) (read year :: Int) 4 /= 0 = day
   | otherwise                                                         = "0"

--------------------------------------------------

----------------
--ACTION: report

report_exp :: [Expense] -> IO String
report_exp state = do
 lwr_bnd <- validDate "Select month to report in format yyyy-mm: " 2

 let lwr_mnth = (!! 1) $ T.splitOn (T.pack "-") $ T.pack lwr_bnd 
 let lwr_year = (!! 0) $ T.splitOn (T.pack "-") $ T.pack lwr_bnd 

 let upr_bnd_month = parse_le10 $ (+1) $ text2int lwr_mnth
 let upr_bnd_year = upr_bnd_year_fun lwr_mnth lwr_year

 let lower_bound =  T.pack $ lwr_bnd ++ "-00"
 let upper_bound = T.pack $ (++) upr_bnd_year $ (++) "-" $ (++) upr_bnd_month "-00"

 -- Make list of expenses between the lower and upper bounds.
 let report =  filter (< upper_Bound {date = upper_bound}) $ filter (>= lower_Bound {date = lower_bound}) state
 -- print report
 putStrLn $ (++) "Total: " $ show $ expense $ calculateAmount report
 -- get all the categories
 let report_categories = list_categories report
 -- print report_categories
 forM_ report_categories $ \ cat -> putStrLn $ (++) (" -- " ++ (T.unpack cat) ++ ": ") $ show $ expense $ category_sum cat report 
 return "-----------Report end-----------\n"

-- HELPER functions

-- Make T.Text to Int
text2int :: T.Text -> Int
text2int x = read ( T.unpack x) :: Int

-- Parse <10 values as '0<nr>'
parse_le10 :: Int -> String
parse_le10 val
 | val < 10  = "0" ++ show val
 | val > 12  = "01"
 | otherwise = show val

-- If lower bound month is 12 (december), then add +1 to year number
upr_bnd_year_fun :: T.Text -> T.Text -> String
upr_bnd_year_fun mnth year
 | mnth > (T.pack "12") = show $ (+1) $ text2int $ year
 | otherwise            = T.unpack year

-- Calculate total expense amount of given list of Expenses.
calculateAmount :: [Expense] -> Expense
calculateAmount xs = foldr (+) 0 xs

-- List unique categories.
list_categories :: [Expense] -> [T.Text]
-- TODO: how to make unique list more elegantly?
list_categories xs = f [] $ [(category x) | x <- xs]
 where
  f zs [] = zs
  f zs (y:ys) 
   | not $ (elem) y zs = f (y:zs) ys
   | otherwise = f zs ys

-- Calculate total amount of given Expenses list by given category.
category_sum :: T.Text -> [Expense] -> Expense
category_sum x ys = calculateAmount $ filterCat x ys
 where
  filterCat x [] = []
  filterCat x (y:ys)
   | (==) x (category y) = y : filterCat x ys
   | otherwise = filterCat x ys

--------------------------------------------------
