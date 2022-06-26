module X.Data.Unit.Metric.MetricSpec where

import Data.Foldable
import Data.Maybe
import Test.Hspec
import X.TestUtils.Should
import X.Data.Unit.Metric.Metric
import X.Data.Unit.Metric.Prefix
import X.Data.Unit.Unit
import X.Data.Unit.UnitLike

spec :: Spec
spec = parallel $ do
    let metricNames p b u = fmap show . snd $ metric p b u

    let scale units name = toScale . fromJust . find ((== name) . show) $ units

    shouldBeSpec
        "metric names shouldBe"
        [ (metricNames (const True) Base (BaseUnit "q"), ["nq", "uq", "mq", "cq", "q", "kq", "Mq", "Gq", "Tq", "Pq", "Eq"], "goes through all prefixes")
        , (metricNames (const True) Kilo (BaseUnit "kq"), ["nq", "uq", "mq", "cq", "q", "kq", "Mq", "Gq", "Tq", "Pq", "Eq"], "goes through all prefixes with kilo base")
        , (metricNames (>= Centi) Base (BaseUnit "q"), ["cq", "q", "kq", "Mq", "Gq", "Tq", "Pq", "Eq"], "goes through all prefixes matching restriction")
        ]

    let bs = snd $ metric (const True) Base (BaseUnit "b")
    let kgs = snd $ metric (const True) Kilo (BaseUnit "kg")

    shouldBeSpec
        "metric scales shouldBe"
        [ (scale bs "b", 1, "b")
        , (scale bs "kb", 1e3, "kb")
        , (scale bs "Mb", 1e6, "Mb")
        , (scale bs "Gb", 1e9, "Gb")
        , (scale bs "Tb", 1e12, "Tb")
        , (scale bs "Pb", 1e15, "Pb")
        , (scale bs "Eb", 1e18, "Eb")
        , (scale kgs "Mg", 1e3, "Mg")
        , (scale kgs "kg", 1, "kg")
        , (scale kgs "g", 1e-3, "g")
        , (scale kgs "cg", 1e-5, "cg")
        , (scale kgs "mg", 1e-6, "mg")
        , (scale kgs "ug", 1e-9, "ug")
        , (scale kgs "ng", 1e-12, "ng")
        ]
