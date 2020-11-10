module MCserial where

import System.Environment -- getArgs
import System.Random -- mkStdGen
-- import Data.Random --package:random-fu
import Data.Random.Normal --package:normaldistribution

-- import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)

s0 :: Double
s0 = 100.0
r :: Double
r = 0.02
t :: Double
t = 0.5
d :: Double
d = 0.03
e :: Double
e = 100
sigma :: Double
sigma = 0.6
alpha :: Double
alpha = 0.05

lower :: Double
lower = 0.0
upper :: Double
upper = 100.0

-- gen :: Int -> Double
-- -- gen x = fst $ random $ mkStdGen x
-- gen x = fst $ normal $ mkStdGen x

gen2 :: [Double] -> Int -> IO [Double]
gen2 xs 0 = return xs
gen2 xs n = do 
 stdnorm <- normalIO
 gen2 (stdnorm:xs) (n-1)

-- randn :: [Double] -> Int -> [Double]
-- randn xs 0 = xs
-- randn xs n = randn (gen (n) : xs) (n-1)


payoff :: Double -> Double -> Double
payoff e s = max (s-e) 0

st :: [Double] -> [Double]
st x =  map (st') x
 where 
 st' rnd = (*) s0 $ exp $ (r-d-sigma^2/2)*t+sigma*(sqrt t)*rnd

bs :: [Double] -> [Double]
bs x = map (\x -> (*) (exp ((-r)*t)) x) $ map (\ s -> payoff e s) $ st x

mean :: (Integer,Double) -> [Double] -> Double
mean (n,summed) []     = summed/(fromIntegral n)
mean (n,summed) (x:xs) = mean (n+1,summed+x) xs 

-- x is randomly generated numbers from some distribution.
mc :: ([Double] -> [Double]) -> [Double] -> Double
mc f x = mean (0,0) $ f x

main = do
 [nstr] <- getArgs
 let n = read nstr :: Int
 x <- gen2 [] $ n
 print $ mc bs x
