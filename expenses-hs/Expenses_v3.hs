module Expenses_v3 where

-- TODOs
-- comment program
-- refactor maybe

import qualified Data.Text as T
import Text.Read -- readMaybe
import Control.Monad -- forM_
import Control.Monad.State as MS -- State
import Csv_parser_v2 -- self written simple parser

dataFile :: String
dataFile = "data.csv"

-- Define new data type Expense
data Expense = E {
   date        :: T.Text
 , expense     :: Double
 , category    :: T.Text
 , description :: T.Text
} deriving (Show,Eq)

-- Define ordering by date only.
instance Ord Expense where
 (<=) E {date = d1} E{date = d2} = (<=) d1 d2
 -- compare E (date _ _ _) (E date2 _ _ _) = if date == date2 then EQ
 -- compare E {date = d1} E{date = d2} = if d1 == d2 then EQ
 --  else if d1 <= d2 then LT
 --  else GT

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
 
default_expense :: Expense
default_expense = E (T.pack "1900-01") 0.0 (T.pack "default") (T.pack "default")

-- Define functions, that make Text to [Int,Double]
text2int :: T.Text -> Int
text2int x = read ( T.unpack x) :: Int

text2double :: T.Text -> Double
text2double x = read ( T.unpack x ) :: Double

-- To make an expense from a list of Text
make_expense :: [T.Text] -> Expense
make_expense xs = E {
   date        = xs !! 0
 , expense     = text2double (xs !! 1)
 , category    = xs !! 2
 , description = xs !! 3
 }

-- Get user input using given prompt.
get_input :: String -> IO (String)
get_input prompt = do
 putStrLn prompt
 input <- getLine
 return input

-- Return help value for the implementation.
print_help :: String --T.Text
print_help = "-- Help -- \n h - show this help \n a - add new expense \n r - report expenses from given \n q - exit program"
-- print_help :: IO ()
-- print_help = putStrLn "-- Help -- \n h - show this help \n a - add new expense \n r - report expenses from given \n q - exit program "

-- validAmount :: String -> IO String
validAmount prompt = do
 amount <- get_input prompt
 let amount_d = readMaybe amount :: Maybe Double
 if amount_d /= Nothing then
   return amount
 else do
  putStrLn "Invalid number"
  validAmount prompt

validDate :: String -> IO String
validDate prompt = do
 date <- get_input prompt
 let parts = map T.unpack $ T.splitOn (T.pack "-") $ T.pack date
 let len_parts = length parts
 -- if the number of elements is wrong, then ask the date again.
 if len_parts > 2 || len_parts < 2 then do
  putStrLn "Invalid date"
  validDate prompt
  -- if the number of elements is correct, then check if year and month numbers and month <= 12.
 else do
  let year  = checkYear (parts !! 0)
  let month = checkMonth (parts !! 1)
  let checkedDate = year ++ "-" ++ month
  -- if checkedDate is not correct, do ask date again.
  if checkedDate /= date then do
   putStrLn "Invalid date"
   validDate prompt
  else do
   return date
 -- TODO: refactor checkYear and checkMonth
 where
  checkYear x = g (readMaybe x :: Maybe Int)
   where 
    g Nothing  = "0"
    g (Just _) = x
  checkMonth x = g (readMaybe x :: Maybe Int)
   where 
    g Nothing  = "0"
    g (Just y)
     | y <= 12 && y>0  = x
     | otherwise       = "0"
 

add_exp :: [Expense] -> IO (String, [Expense])
add_exp state = do
  date        <- validDate "Date in yyyy-mm format:"
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



-- Default lower and upper bounds, that are used to create new bounds when reporting.
lower_Bound :: Expense
lower_Bound = E (T.pack "2020-01-01") 0.0 (T.pack "lower_bound") (T.pack "lower_bound")
upper_Bound :: Expense
upper_Bound = E (T.pack "2020-01-01") 0.0 (T.pack "upper_bound") (T.pack "upper_bound")

-- Parse <10 values as '0<nr>'
parse_le10 :: Int -> String
parse_le10 val
 | val < 10  = "0" ++ show val
 | val > 12  = "01"
 | otherwise = show val

upr_bnd_year_fun :: T.Text -> T.Text -> String
upr_bnd_year_fun mnth year
 | mnth > (T.pack "12") = show $ (+1) $ text2int $ year
 | otherwise            = T.unpack year

calculateAmount :: [Expense] -> Expense
calculateAmount xs = foldr (+) 0 xs

list_categories :: [Expense] -> [T.Text]
-- TODO: Kuidas teha see implementation paremini.
list_categories xs = f [] $ [(category x) | x <- xs]
 where
  f zs [] = zs
  f zs (y:ys) 
   | not $ (elem) y zs = f (y:zs) ys
   | otherwise = f zs ys

category_sum :: T.Text -> [Expense] -> Expense
category_sum x ys = calculateAmount $ filterCat x ys
 where
  filterCat x [] = []
  filterCat x (y:ys)
   | (==) x (category y) = y : filterCat x ys
   | otherwise = filterCat x ys

report_exp :: [Expense] -> IO String
report_exp state = do
 lwr_bnd <- validDate "Select month to report in format yyyy-mm: "

 let lwr_mnth = (!! 1) $ T.splitOn (T.pack "-") $ T.pack lwr_bnd 
 let lwr_year = (!! 0) $ T.splitOn (T.pack "-") $ T.pack lwr_bnd 

 let upr_bnd_month = parse_le10 $ (+1) $ text2int lwr_mnth
 let upr_bnd_year = upr_bnd_year_fun lwr_mnth lwr_year

 let lower_bound =  T.pack $ lwr_bnd ++ "-01"
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

   
loop :: String -> [Expense] -> IO String
loop x state = do
 putStrLn x
 cmd <- get_input "Insert command: "
 parse_cmd cmd state

parse_cmd :: String -> [Expense] -> IO String
parse_cmd cmd state
 | cmd == "q" = return "Closing the program"
 | cmd == "h" = loop print_help state
 | cmd == "a" = add_exp    state >>= (\(prompt,state) -> loop prompt state)
 | cmd == "r" = report_exp state >>= (\prompt -> loop prompt state)
 | otherwise  = loop "Unknown command!" state



----STATE: runState func
--expenseState :: Expense -> State [Expense] [ Expense ]
--expenseState new = do
-- cur <- MS.get
-- put (new:cur)
-- return cur 
 
--getExpenseState cur = do
-- cur <- MS.get
-- return cur

 -- return cur
-- expenseState :: State Int [Expense]
-- expenseState = runState [Expense] []


-- make list of Expenses
-- expenses :: IO String
expenses = do
 content <- parser dataFile
 let state = map make_expense content
 loop print_help state
