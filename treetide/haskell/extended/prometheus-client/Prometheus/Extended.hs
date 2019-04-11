module Prometheus.Extended
    ( module X
    , unsafeCounter
    , unsafeHistogram
    , unsafeSummary
    -- * Convenience types
    , Buckets
    , Quantiles
    -- * Quantiles
    , twoNines
    , threeNines
    -- * Buckets
    , expLinExpBuckets
    , humanLatencyBuckets
    -- * Unexported in original
    , timeAction
    )
where

import           Prometheus                    as X

import           Control.Monad.IO.Class
import           Data.Function                  ( on )
import           Data.Ratio                     ( (%) )
import           Data.Text                      ( Text )
import           System.Clock                   ( Clock(..)
                                                , diffTimeSpec
                                                , getTime
                                                , toNanoSecs
                                                )

type Bucket = Double
type Buckets = [Bucket]
type Quantiles = [Quantile]

-- | Usage:
--
--     {-# NOINLINE myCounter #-}
--     myCounter :: Counter
--     myCounter = unsafeCounter "my_counter" "It counts foobars."
--
{-# NOINLINE unsafeCounter #-}
unsafeCounter :: Text -> Text -> Counter
unsafeCounter name desc = unsafeRegister (counter (Info name desc))

-- | See https://prometheus.io/docs/practices/histograms/ about summary vs
-- histogram. TLDR if multi-instance aggregation is needed, prefer histogram.
{-# NOINLINE unsafeHistogram #-}
unsafeHistogram :: Buckets -> Text -> Text -> Histogram
unsafeHistogram bs name desc = unsafeRegister (histogram (Info name desc) bs)

-- | See https://prometheus.io/docs/practices/histograms/ about summary vs
-- histogram. TLDR if multi-instance aggregation is needed, prefer histogram.
{-# NOINLINE unsafeSummary #-}
unsafeSummary :: Quantiles -> Text -> Text -> Summary
unsafeSummary qs name desc = unsafeRegister (summary (Info name desc) qs)

twoNines, threeNines :: Quantiles
twoNines = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]
threeNines = twoNines ++ [(0.999, 0.0001)]

-- | Creates an exponential bucket with a linear middle section.
expLinExpBuckets :: Bucket -> (Bucket, Bucket) -> Double -> Bucket -> Buckets
expLinExpBuckets start (linStart, linEnd) linWidth end =
    let initial = exponentialBuckets start base (numFactors start linStart)
        mid =
                let linCount = truncate ((linEnd - linStart) / linWidth)
                in  linearBuckets linStart linWidth linCount
        ending = exponentialBuckets linEnd base (numFactors linEnd end)
    in  concat [initial, mid, ending]
  where
    base = 2.0 :: Double
    numFactors from to = truncate (((-) `on` logBase base) to from)

-- | Buckets suitable for latencies that humans are actively waiting on.
-- Has fine sensitivity in the instant-vs-annoying domain of [0..2] seconds.
humanLatencyBuckets :: Buckets
humanLatencyBuckets = expLinExpBuckets 0.0025 (0.1, 2.0) 0.1 32.0

-- | Unexpected timer used by 'observeDuration'. Useful if timing needs to be
-- measured, but then duration is observed by different monitors based on the
-- outcome. Original comment:
--
-- Evaluate @io@ and return its result as well as how long it took to evaluate,
-- in seconds.
timeAction :: MonadIO m => m a -> m (a, Double)
timeAction io = do
    start  <- liftIO $ getTime Monotonic
    result <- io
    end    <- liftIO $ getTime Monotonic
    let duration = toNanoSecs (end `diffTimeSpec` start) % 1000000000
    return (result, fromRational duration)
