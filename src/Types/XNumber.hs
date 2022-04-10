module Types.XNumber where

data XNumber = XInteger Integer | XReal Double
    deriving (Eq, Ord, Show)

instance Num XNumber where
    (XInteger a) + (XInteger b) = XInteger $ a + b
    (XInteger a) + (XReal b) = XReal $ fromInteger a + b
    (XReal a) + (XReal b) = XReal $ a + b
    a + b = b + a
    
    (XInteger a) * (XInteger b) = XInteger $ a * b
    (XInteger a) * (XReal b) = XReal $ fromInteger a * b
    (XReal a) * (XReal b) = XReal $ a * b
    a * b = b * a
    
    (XInteger a) - (XInteger b) = XInteger $ a - b
    (XInteger a) - (XReal b) = XReal $ fromInteger a - b
    (XReal a) - (XReal b) = XReal $ a - b
    a - b = b - a
    
    abs (XInteger a) = XInteger $ abs a
    abs (XReal a) = XReal $ abs a

    signum (XInteger a) = XInteger $ signum a
    signum (XReal a) = XReal $ signum a

    fromInteger = XInteger

