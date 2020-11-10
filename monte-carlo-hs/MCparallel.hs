module MCparallel where

import System.Environment (getArgs)
import Control.Parallel.Strategies
-- import Control.Parallel
import GenUnif
import Function

type Error  = Double
type Result = Double

mean :: (Integer,Double) -> [Double] -> Double
mean (n,summed) []     = summed/(fromIntegral n)
mean (n,summed) (x:xs) = mean (n+1,summed+x) xs 

mc :: (Double -> Double) -> [Double] -> Result
-- mc f xs = mean (0,0) ( runEval $ parMap rseq f xs ) --`using` rpar `using` parBuffer 1000 rseq) 
mc f xs = mean (0,0) $ runEval $ rpar $ map f xs
-- mc f xs = mean (0,0) inparallel 
--  where 
--   inparallel = map f xs `using` parBuffer 100 rseq

-- main :: IO (Result,Error)
main = do
 [nstr] <- getArgs
 let n = read nstr :: Int
 -- let n = 1000
 -- xinit <- rpar (unif n) :: Strategy (IO [Double])
 -- x <- runEval $ rpar (unif n)
 x <- unif n
 -- x <- unif n
 -- let x = xinit `using` parList (rseq :: Strategy Double) 
 let result = mc f x
 let error = abs $ result - analytical
 return (result,error)
 -- if (error > 0.05) then do
 --  putStrLn "Incorrect answer, error > 0.05!"
 -- else do
 --  putStrLn "result,error,analytical"
 --  putStrLn $ (show result) ++ "," ++ (show error)++ "," ++ (show analytical)
