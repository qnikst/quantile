-- |
-- Module: Statistics.Quantile.P2.Interpolation
--
-- Interpolation formulaes, taken from P2 article
module Statistics.P2.Interpolation
  ( parabolic
  , linear
  ) where

-- | Adjust value when moving marker using piecewice parabolic
-- prediction PP $P^2$. The formula assumes that curve passing
-- through any three adjancent markers is a parabola.
--
-- See Appendix Deriviation of the $P^2$ formula for details.
parabolic :: Double -- ^ Height of the left neighbour.
          -> Double -- ^ Height of the current quantile.
          -> Double -- ^ Height of the right neighbour.
          -> Double -- ^ Move Directon (+1 for right, -1 for left)
          -> Int    -- ^ Position of the left neighbour
          -> Int    -- ^ Current position
          -> Int    -- ^ Position of the right neighbour-
          -> Double -- Resulting height.
parabolic q0 qi q1 d n0 ni n1 =
  qi + d / (fromIntegral $ n1 - n0) * ( (fromIntegral (ni - n0) + d) * (q1 - qi) / (fromIntegral $ n1 - ni)
                                      + (fromIntegral (n1 - ni) - d) * (qi - q0) / (fromIntegral $ ni - n0)
                                      )

-- | Adjust value when moving marker using piecewice linear
-- prediction. This formula is needed when prediction using
-- 'parabolic' returns values in decreasing order.
linear :: Double -- ^ Height of the left neighbour.
       -> Double -- ^ Height of the current quantile.
       -> Double -- ^ Height of the right neighbour.
       -> Double -- ^ Move Directon (+1 for right, -1 for left)
       -> Int    -- ^ Position of the left neighbour
       -> Int    -- ^ Current position
       -> Int    -- ^ Position of the right neighbour-
       -> Double -- Resulting height.
linear q0 qi q1 d n0 ni n1
   | d > 0 = qi + (q1 - qi) / (fromIntegral $ n1 - ni)
   | otherwise = qi + (qi - q0)/(fromIntegral $ n0-ni)

