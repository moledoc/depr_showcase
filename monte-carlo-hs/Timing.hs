module Timing where

import Data.Time.Clock
import Data.Time.Calendar

timing :: IO UTCTime
timing = getCurrentTime >>= return

diffTime :: UTCTime -> UTCTime -> Double
-- diffTime end start = (div) (fromEnum (diffUTCTime end start)) 1000000000000
diffTime end start = (fromIntegral (fromEnum $ diffUTCTime end start) :: Double)/1000000000000.0
