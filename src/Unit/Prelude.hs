{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Unit.Prelude (preludeUnits) where

import Unit.Unit
import Unit.Metric.Metric
import Unit.Metric.Prefix
import Unit.UnitScaleOperation
import Unit.Arithmetic

mab = metric (const True) Base

(b, bytes) = metric (>= Base) Base $ BaseUnit "b"

(kg, grams) = metric (const True) Kilo $ BaseUnit "kg"

(m, meters) = mab $ BaseUnit "m"

(s, seconds) = metric (<= Base) Base $ BaseUnit "s"

minute = ScaledUnit "min" (Multiply 60) s

hour = ScaledUnit "h" (Multiply 60) minute

day = ScaledUnit "d" (Multiply 24) hour

week = ScaledUnit "wk" (Multiply 7) day

month = ScaledUnit "mo" (Multiply 30.5) day

year = ScaledUnit "yr" (Multiply 365.24) day

(a, amperes) = mab $ BaseUnit "A"

k = BaseUnit "K"

celsius = ScaledUnit "C" (Add 273.15) k

fahrenheit = ScaledUnit "F" (Add (-32)) (celsius `unitDivScalar` (9/5))

(mol, moles) = mab $ BaseUnit "mol"

(cd, candelas) = mab $ BaseUnit "cd"

(n, newtons) = mab $ ProductUnit "N" [kg, m, s `unitExpScalar` (-2)]

(j, joules) = mab $ ProductUnit "J" [n, s `unitExpScalar` (-1)]

(w, watts) = mab $ ProductUnit "W" [j, s `unitExpScalar` (-1)]

(c, coulombs) = mab $ ProductUnit "C" [a, s]

(v, volts) = mab $ ProductUnit "V" [j, unitReciprocal c]

(o, ohms) = mab $ ProductUnit "Ohm" [v, unitReciprocal a]

(f, farads) = mab $ ProductUnit "F" [c, unitReciprocal v]

(h, henries) = mab $ ProductUnit "H" [o, s]

(hz, hertz) = mab $ ScaledUnit "Hz" (Exponentiate (-1)) s

rad = BaseUnit "rad"

rev = ScaledUnit "rev" (Multiply (2 * pi)) rad

rpm = ProductUnit "rpm" [rev, unitReciprocal minute]

deg = ScaledUnit "deg" (Multiply (pi / 180)) rad

foot = ScaledUnit "ft" (Multiply 0.3048) m

inch = ScaledUnit "in" (Multiply (1/12)) foot

yard = ScaledUnit "yd" (Multiply 3) foot

pound = ScaledUnit "lb" (Multiply 4.448222) n

preludeUnits :: [Unit]
preludeUnits = concat [bytes, grams, meters, seconds, [minute, hour, day, week, month, year], amperes, [k, celsius, fahrenheit], moles, candelas, newtons, joules, watts, coulombs, volts, ohms, farads, henries, hertz, [rad, rev, rpm, deg, foot, inch, yard, pound]]
