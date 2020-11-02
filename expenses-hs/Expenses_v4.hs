module Expenses_v4 where

-- IMPORTS
import Control.Exception
import qualified Data.Text as T --hoogle Data.Text 2020-10-10: This module is intended to be imported qualified, to avoid name clashes with Prelude functions
import qualified Data.Text.IO as T -- use strict (instead of lazy) readFile.
import Text.Read -- readMaybe
import Control.Monad --forM_
import Data.List -- nub
import System.IO --hFlush

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
 show (E da exp cat de) = show da ++ "," ++ show exp ++ "," ++ (T.unpack cat) ++ "," ++ (T.unpack de) -- ++ "\n"

instance Ord Expense where
 (<=) (E {expense = e1}) (E {expense = e2}) = e1 <= e2

data Date = D {
   year  :: Int
 , month :: Int
 , day   :: Int
} deriving (Eq,Ord)

instance Show Date where
 show (D y m d) = show y ++ "-" ++ (lt10 m) ++ show m ++ "-" ++ (lt10 d) ++ show d


-- MAIN FUNCTIONS
main :: IO ()
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
 
-- TODO: error handling when parsing the file.
parseContent :: Monad m => [T.Text] -> m (T.Text, [Expense])
parseContent contents = do
 let header = head contents
 let body   = getBody contents
 return (header,body)
 where
  getBody b = map makeExpenseText $ map (T.splitOn $ T.pack ",") $ filter (\x -> (/=) (T.pack "") $ T.take 1 x) $ tail b

loop :: String -> (T.Text,[Expense]) -> IO ()
loop prompt state = do
 putStrLn prompt
 cmd <- getInput "Insert command: "
 parseCmd cmd state

parseCmd :: String -> (T.Text,[Expense]) -> IO ()
parseCmd cmd state
 | cmd == "q"  = putStrLn "Closing program"
 | cmd == "h"  = loop printHelp state
 | cmd == "a"  = addExp            state >>= (\(prompt,state) -> loop prompt state)
 | cmd == "d" = delExp             state >>= (\(prompt,state) -> loop prompt state)
 | cmd == "rl" = reportShow        state >>= (\prompt -> loop prompt state)
 | cmd == "rr" = reportRange       state >>= (\prompt -> loop prompt state)
 | cmd == "rn" = reportAfterParams state >>= (\prompt -> loop prompt state)
 | cmd == "rc" = reportCategory    state >>= (\prompt -> loop prompt state)
 | otherwise  = loop "Unknown command!" state

printHelp :: String
printHelp = "-- Help -- \n q  - exit program \n h  - show this help \n a  - add new expense \n d  - delete expense \n rl - show last <nr> inserted expenses \n rr - show expenses between range \n rn - report <nr> month expenses \n rc - report expenses of chosen category"

addExp :: (T.Text,[Expense]) -> IO (String, (T.Text,[Expense]))
addExp (header,body) = do
 date        <- validDate "Date in yyyy-mm-dd format: " 3
 expense     <- validAmountD "Expense amount: "
 category    <- getInput "Category of expense: "
 description <- getInput "Description of expense: "
 let newexp = E date expense (T.pack category) (T.pack description)
 let newbody = newexp : body
 appendFile dataFile (show newexp)
 return ("Expense added: " ++ show newexp,(header,newbody))

delExp :: (T.Text,[Expense]) -> IO (String, (T.Text,[Expense]))
delExp (header,body) = do
 (nrOfExp,selection) <- showLastNExpenses body
 nrOfDel <- validAmountI "Delete expense nr: "
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
 return $ "Last " ++ show nrOfExp ++ " inserted expenses shown"

reportRange :: (T.Text,[Expense]) -> IO String
reportRange (header,body) = do
 start <- validDate "Select start date in yyyy-mm-dd format: " 3
 end   <- validDate "Select end date in yyyy-mm-dd format: "   3
 if start>end then do
  putStrLn "Start date > end date"
  reportRange (header,body)
 else do
  showRangeExpenses start end body
  return $ "Expenses between [" ++ show start ++ ", " ++ show end ++ ") shown"

reportAfterParams :: (T.Text,[Expense]) -> IO String
reportAfterParams state = do
 format <- validAmountI "Report with month accuracy (1) or date accuracy (2): "
 if (not $ (elem) format [1,2]) then do
  putStrLn "Invalid choice"
  reportAfterParams state
 else do
   n <- validAmountI "Number of months to be reported: "
   if n < 1 then do
    putStrLn "Invalid number of months"
    reportAfterParams state
   else do
    if format == 1 then
     reportAfter state n (format+1) ""
    else
     reportAfter state n (format+1) "-dd"
     
reportAfter :: (T.Text,[Expense]) -> Int -> Int -> String -> IO String
reportAfter (header,body) n formatI formatS = do
 start <- validDate ("Select month in yyyy-mm"++ formatS ++ " format: ") formatI
 let end = dateAfter start n
 showRangeExpenses start end body
 return $ "Expenses between [" ++ show start ++ ", " ++ show end ++ ") shown"

reportCategory (header,body) = do
 reportinit <- listValues categoryList body "category" returnCategoryExpenses
 selectDescription <- getInput "Select description as well? [y/n]: "
 filterDesc <- reportDescription selectDescription reportinit
 let report = reverse $ sort filterDesc 
 mapM_ putStrLn $ map show report --putStr
 return "Expenses with category (and descrption) shown"

reportDescription :: String -> [Expense] -> IO [Expense]
reportDescription "y" body = listValues descriptionList body "description" returnDescriptionExpenses
reportDescription "n" body = return body
reportDescription _ body   = putStrLn "Invalid option" >> getInput "Select description as well? [y/n]: " >>= (\opt -> reportDescription opt body) 
  

-- HELP FUNCTIONS
lt10 :: Int -> String
lt10 n
 | n < 10    = "0"
 | otherwise = ""

getInput :: String -> IO (String)
getInput prompt = do
 putStr prompt
 hFlush stdout
 getLine

makeExpenseText :: [T.Text] -> Expense
makeExpenseText xs = E{
   date        = makeDateText (xs !! 0)
 , expense     = text2Double (xs !! 1)
 , category    = xs !! 2
 , description = xs !! 3
}

text2Double :: T.Text -> Double
text2Double x = read ( T.unpack x ) :: Double

makeDateInt :: [Int] -> Date
makeDateInt xs = D {
   year  = xs !! 0
 , month = xs !! 1
 , day   = xs !! 2
}

makeDateText :: T.Text -> Date
makeDateText date = D {
    year  = text2Int $ (split 0 date)
  , month = text2Int $ (split 1 date)
  , day   = text2Int $ (split 2 date)
 }
 where
  split n date = (!! n) $ T.splitOn (T.pack "-") date 

text2Int :: T.Text -> Int
text2Int x = read ( T.unpack x) :: Int

calculateExpense :: [Expense] -> Double
calculateExpense xs = foldr (\E {expense = exp} acc -> exp + acc) 0 xs

categoryList :: [Expense] -> [T.Text]
categoryList xs = nub [(category x) | x <- xs]

descriptionList :: [Expense] -> [T.Text]
descriptionList xs = nub [(description x) | x <- xs]

isValidDouble :: String -> Maybe Double
isValidDouble x = readMaybe x :: Maybe Double

isValidInt :: String -> Maybe Int
isValidInt x = readMaybe x :: Maybe Int

validAmountLoop f prompt Nothing  = putStrLn "Invalid number" >> f prompt
validAmountLoop f prompt (Just x) = return x

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

validDate :: String -> Int -> IO Date
validDate prompt n = do
 date <- getInput prompt
 let parts = map T.unpack $ T.splitOn (T.pack "-") $ T.pack date
 if (length parts) /= n then do
  putStrLn "Invalid date format"
  validDate prompt n
 else do
   areValidNumbers <- yyyymmddDate $ map isValidInt parts
   isValidDate <- checkDate areValidNumbers
   if (not isValidDate) then do
    putStrLn "Invalid date"
    validDate prompt n
   else do
    let validDateNumbers = map (\(Just x) -> x) areValidNumbers
    return $ makeDateInt validDateNumbers
   
yyyymmddDate :: [Maybe Int] -> IO [Maybe Int]
yyyymmddDate date = do
 if ( length date ) == 2 then
  return $ date ++ [ Just 1 ]
 else
  return date

checkDate :: [Maybe Int] -> IO Bool
checkDate xs = do
 let isValidYear  = checkYear  (xs !! 0)
 let isValidMonth = checkMonth (xs !! 1)
 let isValidDay   = checkDay   (xs !! 0) (xs !! 1) (xs !! 2)
 if (isValidYear && isValidMonth && isValidDay) then do
  return True
 else do
  return False

checkYear :: Maybe Int -> Bool
checkYear Nothing = False
checkYear (Just year)
 | year > 1900 = True
 | otherwise   = False

checkMonth :: Maybe Int -> Bool
checkMonth Nothing = False
checkMonth (Just month)
 | month <=12 && month > 0 = True
 | otherwise = False

checkDay :: Maybe Int -> Maybe Int -> Maybe Int -> Bool
checkDay Nothing _ _ = False
checkDay _ Nothing _ = False
checkDay _ _ Nothing = False
checkDay (Just year) (Just month) (Just day)
 | day <= 0                                           = False
 | (elem) month [1,3,5,7,8,10,12] && day <= 31        = True
 | (elem) month [4,6,9,11] && day <= 30               = True
 | (elem) month [2] && day <= 29 && (mod) year 4 == 0 = True
 | (elem) month [2] && day < 29  && (mod) year 4 /= 0 = True
 | otherwise                                          = False

dateAfter :: Date -> Int -> Date
dateAfter (D {year = y, month = m, day = d}) n = do
 let month = (mod) (m+n) 12
 let year = (+) y $ max 0 $ (div) (m+n-1) 12
 if month == 0 then
  makeDateInt [year,month+12,d]
 else
  makeDateInt [year,month,d]
  
showLastNExpenses :: [Expense] -> IO (Int, [Expense])
showLastNExpenses body = do
 nr <- validAmountI "Show last <nr> inserted expenses: "
 let nrOfExp = min (length body) nr
 let selection = (take nrOfExp [1..],take nrOfExp body)
 mapM_ (\x -> putStrLn $ show x ++ ": " ++ show ( (!! (x-1)) $ snd selection)) (fst selection) -- putStr
 return (nrOfExp,snd selection)

returnRangeExpenses :: Date -> Date -> [Expense] -> [Expense]
returnRangeExpenses start end body = filter f body
 where
  f exp = g start end exp
  g start end (E {date = d1})
   | start <= d1 && d1 < end = True
   | otherwise               = False

showRangeExpenses :: Date -> Date -> [Expense] -> IO ()
showRangeExpenses start end body = do
  let report = returnRangeExpenses start end body
  let categorylist = categoryList report
  putStrLn $ (++) "Total: " $ show $ calculateExpense report
  -- forM_ categorylist $ (\cat -> putStrLn $ (++) (" -- " ++ (T.unpack cat) ++ ": ") $ show $ calculateExpense $ filter (\ E {category = xcat} -> xcat == cat) report)
  forM_ categorylist $ (\cat -> putStrLn $ (++) (" -- " ++ (T.unpack cat) ++ ": ") $ show $ calculateExpense $ returnCategoryExpenses cat report)

returnCategoryExpenses :: T.Text -> [Expense] -> [Expense]
returnCategoryExpenses cat body = filter f body
 where
  f exp = g cat exp
  g cat (E {category = cat1})
   -- | (T.toUpper cat) == (T.toUpper cat1) = True
   | cat == cat1 = True
   | otherwise   = False

returnDescriptionExpenses :: T.Text -> [Expense] -> [Expense]
returnDescriptionExpenses desc body = filter f body
 where
  f exp = g desc exp
  g cat (E {description = desc1})
   | desc == desc1 = True
   | otherwise     = False

listValues :: (t -> [T.Text]) -> t -> [Char] -> (T.Text -> t -> b) -> IO b
listValues fsel body what ffil = do
 let selection = fsel body
 let nrOfSel = [1..(length selection)]
 mapM_ (\x -> putStrLn $ show x ++ ": " ++ T.unpack ((!! (x-1)) selection)) nrOfSel
 nr <- validAmountI ("Number of "++ what ++ " to be reported: ")
 if nr < 0 || nr > (length selection) then do
  putStrLn "Invalid number"
  listValues fsel body what ffil
 else
  return $ ffil (selection !! (nr-1)) body

