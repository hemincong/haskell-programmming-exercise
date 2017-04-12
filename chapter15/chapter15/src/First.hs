module First where

import Data.Monoid
import Optional

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' (Only x)) _ = First' (Only x)
    mappend _ (First' (Only x)) = First' (Only x)
    mappend _ _  = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend
