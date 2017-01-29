{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (TooMany, Eq, Show)

newtype Goatxx = Goatxx (Int, String) deriving(Eq, Show)

instance TooMany Goatxx where
    tooMany (Goatxx(n, s)) = n > 42

instance TooMany (Int, String) where
    tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
    tooMany (n1, n2) = (n1 + n2)> 42

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)



