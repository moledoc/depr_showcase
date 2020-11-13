module MCparallel where

import System.Environment (getArgs)
import Control.Parallel.Strategies
import Control.Monad

-- self defined
import GenUnif
import Function
import Timing

mc :: (Integer,Double) -> [Double] -> Double
mc (n,summed) []     = summed/(fromIntegral n)
mc (n,summed) (x:xs) = mc (n+1,summed+(f x)) xs 

main = do
 -- read number of mc iterations n and the number that we use to divide n into chunks
 -- [nstr,pstr] <- getArgs
 [nstr] <- getArgs
 let n = read nstr :: Int
 -- start time
 start <- timing
 -- let p = read pstr :: Int
 let p = (div) n (min 5000 n)
 let nchunk = (div) n p
 
 let nchunkd = fromIntegral nchunk
 -- generate list of list with total of n random values from uniform distribution U(0,1)
 
 -- -- FASTER but THEORETICALLY INCORRECT
 -- -- calculate intermediate means.
 -- xs <- unif p
 -- let resultchunk = parMap rseq (\_ -> mc (0,0) xs) $! [1..nchunk] 
 
 -- SLOWER but THEORETICALLY CORRECT
 -- calculate intermediate means.
 xs <- replicateM nchunk $ parUnif p
 let resultchunk = parMap rseq (\ys -> mc (0,0) ys) xs
 
 -- calculate final result.
 -- Since we find means of sublists, then getting the correct final result can be calculated as
 let result = sum(resultchunk)/nchunkd
 -- calculate error
 let error = abs $ result - analytical
 if n > 999 then do
  putStr $ (show n) ++ "," ++ (show result) ++ "," ++ (show analytical) ++ "," ++ (show error) ++ ",parallel,"
  -- end time
  end <- timing
  -- calculate time
  let time = diffTime end start
  putStrLn $ (show time)
 else do
   -- check if error >0.05. If not, then print the result.
   if (error > 0.05) then do
    putStrLn $ "Incorrect answer, error > 0.05! Error: " ++ (show error)
   else do
    putStrLn "result,error,analytical"
    putStrLn $ (show result) ++ "," ++ (show analytical) ++ "," ++ (show error)
