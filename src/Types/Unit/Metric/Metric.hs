module Types.Unit.Metric.Metric where

import Data.List
import Data.Maybe
import Types.Unit.ContextUnit
import Types.Unit.Exponential
import Types.Unit.Metric.Prefix
import Types.Unit.Scale.ScaleSequence
import Types.Unit.Scale.ScaleStep

metric :: (MetricPrefix -> Bool) -> MetricPrefix -> ContextUnit -> (ContextUnit, [ContextUnit])
metric keepPrefix base cu =
    let baseName = fromMaybe (show cu) (stripPrefix (show base) (show cu))
        unitsToList =
            fmap fst
                . filter (keepPrefix . snd)
                . drop 1
                . scanl'
                    ( \(u, p) n ->
                        (ScaledUnit (show n <> show cu) (ScaleSequence [ScaleMultiply $ 10 ** (metricPrefixToExponent n - metricPrefixToExponent p)]) u, n)
                    )
                    (cu, base)
        prev = unitsToList $ drop 1 [base .. minBound]
        succ = unitsToList $ drop 1 [base .. maxBound]
     in (cu, prev <> [cu] <> succ)
