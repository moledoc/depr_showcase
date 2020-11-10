module Function where

-- Define a function, that has some complexity, so that parallel computation might give better results, but is still fairly simple to analytically calculate the exact answer.
f :: Double -> Double
f x = x^2 + x^4 + (sin x) + (cos x) + (x^25)

-- We assume that random variables are taken from uniform distribution U(0,1).
-- This means we can calculate the mean (EX) as integral from 0 to 1 of the function f(x).
analytical :: Double
analytical = 613/390 + (sin 1) - (cos 1)

