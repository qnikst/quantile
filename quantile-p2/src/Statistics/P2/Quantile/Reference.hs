-- |
-- Module: Statistics.Quantile.P2.Reference.
--
-- Reference implementation of the $P^2$ algorithm by RAJ JAIN and IIMRICH CHLAMTAC
-- The $P^2$ Algorithm for Dynamic Statistical Computing Calculation of Quantiles and
-- Without Storing Observations.
--
-- No attempts to provide a fast implementation or sane API were made.
module Statistics.P2.Quantile.Reference where

import Statistics.P2.Interpolation

import Data.Foldable
import Control.Monad.Trans
import Control.Monad.Trans.State

newtype M = M { runM :: Double -> (M, Double, ([Int], [Double], [Int], [Double])) }

-- | Initialize counter.
-- @x0 <= x1 <= x2 <= x3 <= x4@ should be hold.
initialize :: Double -- Quantile to measure
           -> Double -- x0
           -> Double -- x1
           -> Double -- x2
           -> Double -- x3
           -> Double -- x4
           -> M
initialize p x1 x2 x3 x4 x5 = M (go qs ns n's)
  where
    qs   = [x1,    x2,     x3,     x4, x5]  -- marker height
    ns   = [ 1,     2,     3,       4,  5]  -- marker positions
    n's  = [ 1, 1+2*p, 1+4*p,   3+2*p,  5]  -- marker desired positions
    dn's = [ 0,   p/2,     p, (1+p)/2,  1]  -- increment
    go qs@[_q_1, q_2, q_3, q_4, _q_5]
       ns
       n's
       x_j = (M (go qs' ns' n's'), q_3', ([n_1, n_2, n_3, n_4, n_5], n's', [n_1', n_2', n_3', n_4', n_5'], qs'))
       where
        -- 1. find cell k such that q_k <= x_j <= q_{k+1} and adjust extreme values if nesessary
        (k, q_1, q_5) = case () of
            _ | x_j <  _q_1 -> (1,  x_j, _q_5)
              | x_j <   q_2 -> (1, _q_1, _q_5)
              | x_j <   q_3 -> (2, _q_1, _q_5)
              | x_j <   q_4 -> (3, _q_1, _q_5)
              | x_j <= _q_5 -> (4, _q_1, _q_5)
              | otherwise   -> (4, _q_1,  x_j)
        -- 2. increment positions of markers k+1 through 5
        [n_1, n_2, n_3, n_4, n_5] = zipWith (\i n -> if i >= k+1 then n+1 else n) [1..5] ns
        [n'_1, n'_2, n'_3, n'_4, n'_5] = zipWith (+) n's dn's
        -- 3. adjust heights if nesessary
        (q_1', n_1') = (q_1, n_1)
        (q_2', n_2') = adjustHeight n_1' n_2 n_3 n'_2 q_1' q_2 q_3
        (q_3', n_3') = adjustHeight n_2' n_3 n_4 n'_3 q_2' q_3 q_4
        (q_4', n_4') = adjustHeight n_3' n_4 n_5 n'_4 q_3' q_4 q_5
        (q_5', n_5') = (q_5, n_5)
        qs'  = [q_1', q_2', q_3', q_4', q_5']
        ns'  = [n_1', n_2', n_3', n_4', n_5']
        n's' = [n'_1, n'_2, n'_3, n'_4, n'_5]

-- | Adjust height of the measurement. Returns new height and new position
-- of the element.
adjustHeight :: Int -- ^ Position of the left neighbour.
             -> Int -- ^ Position of the current element.
             -> Int -- ^ Position of the right neighbour.
             -> Double -- ^ Desired position
             -> Double -- ^ Height of the left neighbour.
             -> Double -- ^ Height of the current element.
             -> Double -- ^ Height of the right neighbour.
             -> (Double, Int)
adjustHeight n0 ni n1 n' q0 qi q1
  | (d >= 1 && n1 - ni > 1) || (d <= -1 && n0 - ni < (-1)) =
    let di = signum d
        q' = parabolic q0 qi q1 di n0 ni n1
    in if q0 < q' && q' < q1
       then (q', ni+round di)
       else (linear q0 qi q1 di n0 ni n1, ni + round di)
  | otherwise = (qi, ni)
  where
    d = n' - fromIntegral ni

test = do
  let q0 = initialize 0.5 0.02 0.5 0.74 0.83 3.39
  flip runStateT q0 $ do
    for_ [22.37, 10.37, 15.43, 38.62, 15.92, 34.60, 10.28, 1.47, 0.40, 0.05, 11.39, 0.27, 0.42, 0.09, 11.37] $ \x -> do
      q <- get
      let (q', r, a) = runM q x
      lift $ print (r, a)
      put q'
