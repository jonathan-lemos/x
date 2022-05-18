module Types.Unit.Utils where

import Types.Unit.Unit

metric :: Unit -> [Unit]
metric u =
    let kilo = DerivedUnit ("k" <> unitName u) 1000 [(u, 1)]
        mega = DerivedUnit ("M" <> unitName u) 1000 [(kilo, 1)]
        giga = DerivedUnit ("G" <> unitName u) 1000 [(mega, 1)]

        centi = DerivedUnit ("c" <> unitName u) (1/100) [(u, 1)]
        milli = DerivedUnit ("m" <> unitName u) (1/1000) [(u, 1)]
        micro = DerivedUnit ("u" <> unitName u) (1/1000) [(milli, 1)]
        pico = DerivedUnit ("u" <> unitName u) (1/1000) [(micro, 1)]

        in [
            u,
            kilo,
            mega,
            giga,
            centi,
            milli,
            micro,
            pico
        ]