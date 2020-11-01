module Expenses where

-- IMPORTS

import Control.Exception
import qualified Data.Text as T --hoogle Data.Text 2020-10-10: This module is intended to be imported qualified, to avoid name clashes with Prelude functions
import qualified Data.Text.IO as T -- use strict (instead of lazy) readFile.
import Text.Read -- readMaybe
import Control.Monad --forM_

-- DEFINITIONS
dataFile :: FilePath
dataFile = "data.csv"

dataHeader :: String
dataHeader = "Date,Expense,Category,Description\n"

data Expense = E {
   date        :: Date
 , expense     :: Double
 , category    :: T.Text
 , description :: T.Text
} deriving (Eq)

instance Show Expense where
 show (E da exp cat de) = show da ++ "," ++ show exp ++ "," ++ (T.unpack cat) ++ "," ++ (T.unpack de) ++ "\n"

data Date = D {
   year  :: Int
 , month :: Int
 , day   :: Int
} deriving (Eq)

instance Show Date where
 -- show (D y m Nothing)  = show y ++ "-" ++ show m
 -- show (D y m (Just d)) = show y ++ "-" ++ show m ++ "-" ++ show d
 show (D y m d) = show y ++ "-" ++ (lt10 m) ++ show m ++ "-" ++ (lt10 d) ++ show d

-- MAIN FUNCTIONS

main :: IO String
main = do
 rawcontent <- readData
 parsedcontent <- parseContent $ T.lines rawcontent
 loop printHelp parsedcontent

readData :: IO T.Text
readData = do
 result <- try (T.readFile dataFile) :: IO (Either SomeException T.Text)
 case result of
  Left err -> (putStrLn $ "Could not find file \""++dataFile++"\"!!! Creating data.csv") >> writeFile dataFile dataHeader >> return (T.pack "")
  Right contents -> putStrLn "Data read in!" >> return contents
 
parseContent :: Monad m => [T.Text] -> m (T.Text, [Expense])
parseContent contents = do
 let header = head contents
 let body   = getBody contents
 return (header,body)
 where
  getBody b = map makeExpense $ map (T.splitOn $ T.pack ",") $ filter (\x -> (/=) (T.pack "") $ T.take 1 x) $ tail b


loop :: String -> (T.Text,[Expense]) -> IO String
loop prompt state = do
 putStrLn prompt
 cmd <- getInput "Insert command: "
 parseCMD cmd state

parseCMD :: String -> (T.Text,[Expense]) -> IO String
parseCMD cmd state
 | cmd == "q"  = return "Closing the program"
 | cmd == "h"  = loop printHelp state
 -- | cmd == "a"  = addExp     state >>= (\(prompt,state) -> loop prompt state)
 | cmd == "rs" = reportShow state >>= (\prompt -> loop prompt state)
 -- | cmd == "r" = reportExp state >>= (\prompt -> loop prompt state)
 | cmd == "d" = delExp    state   >>= (\(prompt,state) -> loop prompt state)
 | otherwise  = loop "Unknown command!" state

printHelp :: String
printHelp = "-- Help -- \n h - show this help \n a - add new expense \n rs - show last inserted expenses \n r - report expenses \n d - delete expense \n q - exit program"

-- addExp :: (T.Text,[Expense]) -> IO (String, (T.Text,[Expense]))
-- addExp (header,body) = do
--  date        <- validDate "Date in yyyy-mm-dd format:" 3
--  expense     <- validAmountD "Expense amount:"
--  category    <- T.pack $ getInput "Category of expense:"
--  description <- T.pack $ getInput "Description of expense:"
--  -- let newexp = makeExpense $ map T.pack [date,expense,category,description]
--  let newexp = E date expense category description
--  let newbody = newexp : body
--  appendFile dataFile (show newexp)
--  -- return ("Expense added: " ++ show newexp,(header,newexp : body))
--  return ("Expense added: " ++ show newexp,(header:newbody))

delExp :: (T.Text,[Expense]) -> IO (String, (T.Text,[Expense]))
delExp (header,body) = do
 (nrOfExp,selection) <- showLastNExpenses body
 nrOfDel <- validAmountI "Delete <nr> expense: "
 if nrOfDel >= nrOfExp  || nrOfDel < 0 then do
  putStrLn "Invalid number"
  delExp (header,body)
 else do
  let exclude = selection !! nrOfDel
  let newbody = filter (/= exclude) body
  writeFile dataFile dataHeader
  forM_ newbody $ (\line -> appendFile dataFile (show line))
  return ("Expense deleted",(header,newbody))

reportShow :: (T.Text,[Expense]) -> IO String
reportShow (header,body) = do
 (nrOfExp,selection) <- showLastNExpenses body
 return $ "End of last " ++ show nrOfExp ++ "expenses inserted"

-- reportRange :: (T.Text,[Expense]) -> IO (String, (T.Text,[Expense]))
-- reportRange (header,body) = do
--  start <- validDate "Select start date in yyyy-mm-dd format:" 3
--  end   <- validDate "Select end date in yyyy-mm-dd format:"   3
--  if start>end then do
--   putStr "Invalid start date"
--   reportRange (header,body)
--  else do
--   let lowerbound = makeDate start
--   let upperbound = makeDate end
--   print "tmp"


-- HELP FUNCTIONS

lt10 :: Int -> String
lt10 n
 | n < 10    = "0"
 | otherwise = ""

getInput :: String -> IO (String)
getInput prompt = do
 putStrLn prompt
 input <- getLine
 return input

makeExpense :: [T.Text] -> Expense
makeExpense xs = E{
   date        = makeDate (xs !! 0)
 , expense     = text2Double (xs !! 1)
 , category    = xs !! 2
 , description = xs !! 3
}

text2Double :: T.Text -> Double
text2Double x = read ( T.unpack x ) :: Double

makeDate :: T.Text -> Date
makeDate date = D {
    year  = text2Int $ (split 0 date)
  , month = text2Int $ (split 1 date)
  , day   = text2Int $ (split 2 date)
 }
 where
  split n date = (!! n) $ T.splitOn (T.pack "-") date 

text2Int :: T.Text -> Int
text2Int x = read ( T.unpack x) :: Int

showLastNExpenses :: [Expense] -> IO (Int, [Expense])
showLastNExpenses body = do
 nr <- validAmountI "Show last <nr> inserted expenses:"
 let nrOfExp = min (length body) nr
 let selection = (take nrOfExp [0..],take nrOfExp body)
 mapM_ (\x -> putStr $ show x ++ ": " ++ show ( (!! x) $ snd selection)) (fst selection)
 return (nrOfExp,snd selection)

isValidDouble :: String -> Maybe Double
isValidDouble x = readMaybe x :: Maybe Double
isValidInt :: String -> Maybe Int
isValidInt x = readMaybe x :: Maybe Int

validAmountLoop f prompt Nothing  = putStrLn "Invalid number" >> f prompt
validAmountLoop f prompt (Just x) = return x

validAmountStatic Nothing  = Nothing
validAmountStatic (Just x) = x

validAmountD :: String -> IO Double
validAmountD prompt = do
 amount <- getInput prompt
 let maybedouble = isValidDouble amount
 validAmountLoop validAmountD prompt maybedouble


validAmountI :: String -> IO Int
validAmountI prompt = do
 amount <- getInput prompt
 let maybeint = readMaybe amount :: Maybe Int
 let maybedouble = isValidDouble amount
 validAmountLoop validAmountI prompt maybeint



-- validDate :: String -> Int -> IO Date
validDate prompt n = do
 date <- getInput prompt
 let parts = map T.unpack $ T.splitOn (T.pack "-") $ T.pack date
 -- print parts
 -- forM_ areValidNumber $ (\p -> print $ show p)
 let areValidNumbers = map isValidInt parts
 print areValidNumbers
 print ""
 -- let partslength = length parts
 -- -- if the number of elements is wrong, then ask the date again.
 -- if partslength /= n then do
 --  putStrLn "Invalid date"
 --  validDate prompt n
 -- else do
 --  -- if checkDate is not correct, ask date again.
 --  if (checkDate parts n) /= date then do
 --   putStrLn "Invalid date"
 --   validDate prompt n
 --  else
 --   return date

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

