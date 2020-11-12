module MCserial where

import System.Environment (getArgs)

-- self defined
import GenUnif
import Function
import Timing

mc :: (Integer,Double) -> [Double] -> Double
mc (n,summed) []     = summed/(fromIntegral n)
mc (n,summed) (x:xs) = mc (n+1,summed+(f x)) xs 

main = do
 -- let n = 10000
 -- read number of mc iterations n.
 [nstr] <- getArgs
 let n = read nstr :: Int
 -- start time
 start <- timing
 -- generate list of n random values from uniform distribution U(0,1).
 xs <- unif n
 -- calculate mc result
 let result = mc (0,0) xs
 -- calculate error
 let error = abs $ result - analytical
 if n > 999 then do
  putStr $ (show n) ++ "," ++ (show result) ++ "," ++ (show analytical) ++ "," ++ (show error) ++ ",serial,"
  -- end time
  end <- timing
  -- calculate time
  let time = diffTime end start
  putStrLn $ (show time)
 else do
   -- check if error >0.05. If not, then print the result.
   if (error > 0.05) then do
    putStrLn "Incorrect answer, error > 0.05!"
   else do
    putStrLn "result,error,analytical"
    putStrLn $ (show result) ++ "," ++ (show analytical) ++ "," ++ (show error)
