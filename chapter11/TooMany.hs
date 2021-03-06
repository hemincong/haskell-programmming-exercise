{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- for exrcises:  Logic Goats

module TooMany where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42 

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
    tooMany (Goats n) = tooMany n

instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
    tooMany (n1, n2) = tooMany n1 && tooMany n2

instance  (Num a, TooMany a) => TooMany(a, a) where
    tooMany (a, b) = tooMany a || tooMany b



