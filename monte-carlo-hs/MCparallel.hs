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
 -- In my testing I found that best speedup is achieved, if n/p is in the interval [25,50].
 -- However, using n/p=100 gives more stable answer, meaning error more than 0.05 is less likely. Also, correct answer is calculated in case of with smaller n.
 -- [nstr,pstr] <- getArgs
 [nstr] <- getArgs
 let n = read nstr :: Int
 -- start time
 start <- timing
 -- let p = read pstr :: Int
 let p = (div) n 50
 let nchunk = (div) n p
 -- make double nchuck value.
 -- Since we find means of sublists, then getting the correct final result can be calculated as
 -- sum(intermediate means)/nchuck
 let nchunkd = fromIntegral nchunk
 -- generate list of list with total of n random values from uniform distribution U(0,1)
 xs <- unif p
 -- xs <- replicateM nchunk $! unif p
 -- calculate intermediate means.
 let resultchunk = parMap rseq (\_ -> mc (0,0) xs) $! [1..nchunk] 
 -- let resultchunk = parMap rseq (\ys -> mc (0,0) ys) xs
 -- calculate final result.
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
