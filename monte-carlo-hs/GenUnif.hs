module GenUnif where

import Control.Monad (replicateM)
import Control.Monad.Random (uniform)

size :: Double
size = 1000000

lower :: Double
lower = 0
upper :: Double
upper = 1
step :: Double
step = 0.001

valList :: [Double]
valList = [lower,lower+step..upper]

unif :: Int -> IO [Double]
unif n = replicateM n $ uniform $! valList
