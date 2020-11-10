module MCserial where

import System.Environment (getArgs)
import GenUnif
import Function

type Error = Double
type Result = Double

mean :: (Integer,Double) -> [Double] -> Double
mean (n,summed) []     = summed/(fromIntegral n)
mean (n,summed) (x:xs) = mean (n+1,summed+x) xs 

mc :: (Double -> Double) -> [Double] -> Result
mc f xs = mean (0,0) $ map f xs

main :: IO (Result,Error)
main = do
 [nstr] <- getArgs
 let n = read nstr :: Int
 -- let n = 10000
 x <- unif n
 let result = mc f x
 let error = abs $ result - analytical
 return (result,error)
 -- if (error > 0.05) then do
 --  putStrLn "Incorrect answer, error > 0.05!"
 -- else do
 --  putStrLn "result,error,analytical"
 --  putStrLn $ (show result) ++ "," ++ (show error)++ "," ++ (show analytical)
-- -- putStrLn $ "result: " ++ (show result) ++ ", error: " ++ (show error)++ ", analytical: " ++ (show analytical)
