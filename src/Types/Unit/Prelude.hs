module Types.Unit.Prelude (preludeUnits) where

import Types.Unit.Unit
import Data.Map

b = BaseUnit "b"
kb = DerivedUnit "kb" 1000 [(b, 1)]
mb = DerivedUnit "mb" 1000 [(kb, 1)]
gb = DerivedUnit "gb" 1000 [(mb, 1)]
tb = DerivedUnit "tb" 1000 [(gb, 1)]

kg = BaseUnit "kg"
g = DerivedUnit "g" (1 / 1000) [(kg, 1)]
mg = DerivedUnit "mg" (1 / 1000) [(g, 1)]
ug = DerivedUnit "ug" (1 / 1000) [(mg, 1)]

m = BaseUnit "m"
km = DerivedUnit "km" 1000 [(m, 1)]
cm = DerivedUnit "cm" (1 / 100) [(m, 1)]
mm = DerivedUnit "cm" (1 / 1000) [(m, 1)]
um = DerivedUnit "cm" (1 / 1000) [(mm, 1)]

s = BaseUnit "s"

n = DerivedUnit "N" 1 [(kg, 1), (m, 1), (s, -2)]

preludeUnits :: Map String Unit
preludeUnits =
    [ b
    , kb
    , mb
    , gb
    , tb
    , kg
    , g
    , mg
    , ug
    , m
    , km
    , cm
    , mm
    , um
    , s
    , n
    ]
