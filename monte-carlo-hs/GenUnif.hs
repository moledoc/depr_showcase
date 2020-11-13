module GenUnif where

import Control.Monad (replicateM)
import Control.Monad.Random (uniform)
import Control.Parallel.Strategies

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

parUnif :: Int -> IO [Double]
parUnif p = replicateM p $ uniform $ (using) valList (parBuffer 1000 rseq)
