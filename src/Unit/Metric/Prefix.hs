module Unit.Metric.Prefix where

data MetricPrefix = Nano | Micro | Milli | Centi | Base | Kilo | Mega | Giga | Tera | Peta | Exa
    deriving (Eq, Ord, Enum, Bounded)

instance Show MetricPrefix where
    show p = case p of
        Nano -> "n"
        Micro -> "u"
        Milli -> "m"
        Centi -> "c"
        Base -> ""
        Kilo -> "k"
        Mega -> "M"
        Giga -> "G"
        Tera -> "T"
        Peta -> "P"
        Exa -> "E"

metricPrefixToExponent :: (Num a) => MetricPrefix -> a
metricPrefixToExponent p =
    case p of
        Nano -> -9
        Micro -> -6
        Milli -> -3
        Centi -> -2
        Base -> 0
        Kilo -> 3
        Mega -> 6
        Giga -> 9
        Tera -> 12
        Peta -> 15
        Exa -> 18
