-- |
-- Calculate b-cell histogram.
module Statistics.P2.Histogram
  ( initialize
  ) where

import Data.List as List
import Statistics.P2.Interpolation

newtype M = M { runM :: Double -> (M, [Double]) }

initialize :: Int -> [Double] -> M
initialize b ys = M ( go b xs ni )
  where
    xs = sort ys
    ni = [1..b+1]

go :: Int -> [Double] -> [Int] -> Double -> (M, [Double])
go b qs@(q_0:_) ns x = (M $ go b qs' ns', qs') where
  mi = findIndex (\(q0, q1) -> q0 <= x && x < q1) $ zip qs (tail qs)
  (k, qs') = case mi of
               _ | x < q_0 -> (1, x:tail qs)
               Just n  -> (n+1, qs)
               Nothing -> (b+1, init qs ++ [x])
  ns' = zipWith (\i n -> if i >= k+1 then n+1 else n) [1..] ns
  qn@((y1,n1):_) = zip qs' ns'
  l = last qn
  (qs'',ns'') = unzip $
    (y1,n1) :
    zipWith4 (\i (q0,n0) (q,n) (q1,n1) -> adjustHeight b i n0 n n1 q0 q q1) [2..] qn (tail qn) (tail (tail qn))
    ++ [l]

adjustHeight :: Int -> Int -> Int -> Int -> Int -> Double -> Double -> Double -> (Double, Int)
adjustHeight b i n0 n n1 q0 q q1
    | (d >= 1 && n1 - n > 1) || (d <= -1 && n0 - n1 <= (-1))
       = let ds = signum d
             q' = parabolic q0 q q1 ds n0 n n1
         in if q0 < q && q < q1
            then (q, n + round ds)
            else (linear q0 q q1 ds n0 n n1, n + round ds)
    | otherwise = (q, n)
  where
    n' = 1 + (fromIntegral $ (i-1) * (n-1)) / (fromIntegral b)
    d  = n' - fromIntegral n
