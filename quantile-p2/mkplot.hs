{-# LANGUAGE LambdaCase #-}
import Statistics.P2.Quantile.Reference
import Statistics.Quantile as Q
import Data.List
import Data.Word
import Data.Foldable
import GHC.RTS.Events
import System.Exit
import System.Environment
import Text.Printf
import Data.Bifunctor


import qualified Data.Vector.Unboxed as V

data S = NoGC
       | RunGC Word64

main = do
  [fn] <- getArgs
  readEventLogFromFile fn >>= \case
    Left e -> die e
    Right log -> do
      let (times, xss@(x1:x2:x3:x4:x5:xs)) = unzip $ map (second fromIntegral) $ analyze NoGC (sortedEvents log)
          t0 = initialize 0.99 x1 x2 x3 x4 x5
          points = drop 95 $ snd $ mapAccumL (\t d -> let (t', q, _) = runM t d
                                            in (t', q)) t0 xs
          reals = drop 100 $ map (Q.weightedAvg 99 100 . V.fromList) $ inits xss
          tim   = drop 100 $ times
      for_ (zip4 [100..] tim points reals) $ \(n,t, q, r) -> do
        printf "%d %d %f %f\n" (n::Int) t q r
  where
    analyze s ((Event time spec):es) = case spec of
      StartGC -> analyze (RunGC time) es
      EndGC -> case s of
        RunGC ns -> (ns, time-ns) : analyze NoGC es
        NoGC -> error "impossible happened"
      _ -> analyze s es
    analyze s [] = []


sortedEvents :: EventLog -> [Event]
sortedEvents (EventLog _header (Data es)) = map ce_event (sortEvents es)
