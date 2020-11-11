module MCconcurrent where

import System.Environment (getArgs)
import Control.Concurrent
import GenUnif
import Function

type Error = Double
type Result = Double

mean :: (Integer,Double) -> [Double] -> Double
mean (n,summed) []     = summed/(fromIntegral n)
mean (n,summed) (x:xs) = mean (n+1,summed+x) xs 

mc :: (Double -> Double) -> [Double] -> Chan [Double] -> IO ()
mc f xs endFlags = do
 writeChan endFlags $! map f xs

main :: IO (Result,Error)
main = do
 mVar <- newEmptyMVar
 endFlags <- newChan
 [nstr] <- getArgs
 let n = read nstr :: Int
 let p = 8
 -- let n = 10000
 x <- unif n
 mapM_ (const $ forkIO $ mc f x endFlags) [1..p]
 resultinit <- readChan endFlags
 let result = mean (0,0) resultinit
 let error = abs $ result - analytical
 return (result,error)
 -- if (error > 0.05) then do
 --  putStrLn "Incorrect answer, error > 0.05!"
 -- else do
 --  putStrLn "result,error,analytical"
 --  putStrLn $ (show result) ++ "," ++ (show error)++ "," ++ (show analytical)
-- -- putStrLn $ "result: " ++ (show result) ++ ", error: " ++ (show error)++ ", analytical: " ++ (show analytical)
