module Types.Unit.Prelude (preludeUnits) where

import Types.Unit.ContextUnit
import Data.Map
import Types.Unit.BaseUnit
import Types.Unit.Metric.Metric
import Types.Unit.Metric.Prefix
import Types.Unit.Scale.ScaleStep (ScaleStep(ScaleMultiply, ScaleAdd))
import Types.Unit.Scale.ScaleSequence
import Types.Unit.Exponential

_lit :: String -> ContextUnit
_lit = CtxBaseUnit . BaseUnit

mab = metric (const True) Base

(b, bytes) = metric (>= Base) Base $ _lit "b"

(kg, grams) = metric (const True) Kilo $ _lit "kg"

(m, meters) = mab $ _lit "m"

(s, seconds) = metric (<= Base) Base $ _lit "s"

minute = ScaledUnit "min" (ScaleSequence [ScaleMultiply 60]) s

hour = ScaledUnit "h" (ScaleSequence [ScaleMultiply 60]) minute

day = ScaledUnit "d" (ScaleSequence [ScaleMultiply 24]) hour

week = ScaledUnit "wk" (ScaleSequence [ScaleMultiply 7]) day

month = ScaledUnit "mo" (ScaleSequence [ScaleMultiply 30.5]) day

year = ScaledUnit "yr" (ScaleSequence [ScaleMultiply 365.24]) day

(a, amperes) = mab $ _lit "A"

k = BaseUnit "°K"

celsius = ScaledUnit "°C" (ScaleSequence [ScaleAdd 273.15]) (CtxBaseUnit k)

fahrenheit = ScaledUnit "°F" (ScaleSequence [ScaleMultiply (9/5), ScaleAdd 32]) celsius

(mol, moles) = mab $ _lit "mol"

(cd, candelas) = mab $ _lit "cd"

(n, newtons) = mab $ ProductUnit "N" [Exponential kg 1, Exponential m 1, Exponential s (-2)]

(j, joules) = mab $ ProductUnit "J" [Exponential n 1, Exponential s (-1)]

(w, watts) = mab $ ProductUnit "W" [Exponential j 1, Exponential s (-1)]

(c, coulombs) = mab $ ProductUnit "C" [Exponential a 1, Exponential s 1]

(v, volts) = mab $ ProductUnit "V" [Exponential j 1, Exponential c (-1)]

(o, ohms) = mab $ ProductUnit "Ohm" [Exponential v 1, Exponential a (-1)]

(f, farads) = mab $ ProductUnit "F" [Exponential c 1, Exponential v (-1)]

(h, henries) = mab $ ProductUnit "H" [Exponential o 1, Exponential s 1]

(hz, hertz) = mab $ ProductUnit "Hz" [Exponential s (-1)]

rad = BaseUnit "rad"

rev = ScaledUnit "rev" (ScaleSequence [ScaleMultiply (2 * pi)]) (CtxBaseUnit rad)

rpm = ProductUnit "rpm" [Exponential rev 1, Exponential minute (-1)]

deg = ScaledUnit "deg" (ScaleSequence [ScaleMultiply (pi / 180)]) (CtxBaseUnit rad)

foot = ScaledUnit "ft" (ScaleSequence [ScaleMultiply 0.3048]) m

inch = ScaledUnit "in" (ScaleSequence [ScaleMultiply (1/12)]) foot

yard = ScaledUnit "yd" (ScaleSequence [ScaleMultiply 3]) foot

pound = ScaledUnit "lb" (ScaleSequence [ScaleMultiply 4.448222]) n

preludeUnits :: [ContextUnit]
preludeUnits = concat [bytes, grams, meters, seconds, [minute, hour, day, year], amperes, [CtxBaseUnit k, celsius, fahrenheit], moles, candelas, newtons, joules, watts, coulombs, volts, ohms, farads, henries, hertz, [CtxBaseUnit rad, rev, rpm, deg, foot, inch, yard, pound]]
