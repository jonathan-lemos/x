module X.Data.Unit.Metric.Metric where

import Data.List
import Data.Maybe
import X.Data.Unit.Unit
import X.Data.Unit.Metric.Prefix
import X.Data.Unit.UnitScaleOperation

metric :: (MetricPrefix -> Bool) -> MetricPrefix -> Unit -> (Unit, [Unit])
metric keepPrefix base cu =
    let baseName = fromMaybe (show cu) (stripPrefix (show base) (show cu))
        unitsToList =
            fmap fst
                . filter (keepPrefix . snd)
                . drop 1
                . scanl'
                    ( \(u, p) n ->
                        (ScaledUnit (show n <> baseName) (Multiply $ 10 ** (metricPrefixToExponent n - metricPrefixToExponent p)) u, n)
                    )
                    (cu, base)
        prev = unitsToList [minBound .. pred base]
        post = unitsToList [succ base .. maxBound]
     in (cu, prev <> [cu] <> post)
