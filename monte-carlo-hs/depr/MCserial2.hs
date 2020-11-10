module MCserial where

-- import System.Environment -- getArgs
import System.Random 

type Error  = Double
type Result = Double

lbound :: Double
lbound = 0
ubound :: Double
ubound = 100

unif :: [Double] -> Int -> IO [Double]
unif xs 0 = return xs
unif xs n = do 
 stdnorm <- randomRIO (lbound,ubound)
 unif ((stdnorm/ubound):xs) (n-1)

mean :: (Integer,Double) -> [Double] -> Double
mean (n,summed) []     = summed/(fromIntegral n)
mean (n,summed) (x:xs) = mean (n+1,summed+x) xs 

mc :: (Double -> Double) -> [Double] -> Result
mc f xs = mean (0,0) $ map f xs

f :: Double -> Double
f x = x^2

analytical :: Double
analytical = 1/3

main = do
 -- [nstr] <- getArgs
 -- let n = read nstr :: Int
 let n = 1000000
 x <- unif [] n
 let result = mc f x
 let error = abs $ result - analytical
 print (result,error)
