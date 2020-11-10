module MCparallel where

-- import System.Environment -- getArgs
import System.Random 
import Control.Parallel.Strategies

type Error  = Double
type Result = Double

lbound :: Double
lbound = 0
ubound :: Double
ubound = 100


 
unifMList :: Int -> IO [Double]
unifMList n = replicateM n (randomRIO (lbound,ubound)) `using` rpar --parBuffer 100 (rseq :: Strategy (Double))


unifList :: [Double] -> Int -> IO [Double]
unifList xs 0 = return xs
unifList xs n = do 
 stdnorm <- randomRIO (lbound,ubound) 
 unifList ((stdnorm/ubound):xs) (n-1)

unif :: IO Double
unif = do 
 stdnorm <- randomRIO (lbound,ubound) 
 let result = stdnorm/ubound
 return result


mean :: (Integer,Double) -> [Double] -> Double
mean (n,summed) []     = summed/(fromIntegral n)
mean (n,summed) (x:xs) = mean (n+1,summed+x) xs 

mc :: (Double -> Double) -> [Double] -> Result
mc f xs = mean (0,0) ( map f xs `using` rpar ) --`using` parBuffer 1000 rseq) 

f :: Double -> Double
f x = x^2

analytical :: Double
analytical = 1/3

main = do
 -- [nstr] <- getArgs
 -- let n = read nstr :: Int
 let n = 1000000
 --
 xinit <- mapM (\_ -> unif) [1..n]
 let x = x `using` parList (rseq :: Strategy Double) 
 --
 -- x <- unifList [] n `using` (parBuffer 100 (rseq::Strategy Double)) :: Strategy (IO [Double])
 -- x <- mapM (\_ -> (randomRIO (lbound,ubound))) [1..n] `using` (parBuffer 100 (rseq)) 
 -- x <- parMap rseq (\_ -> unif) [1..n] 
 let result = mc f x
 let error = abs $ result - analytical
 print (result,error)
