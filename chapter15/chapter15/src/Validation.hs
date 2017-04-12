module Validation where

import Data.Semigroup

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b ) where
    (Failure a1) <> (Failure a2) = (Failure (a1 <> a2))
    (Failure a1) <> _ = (Failure a1) 
    _ <> (Failure a1) = (Failure a1) 
    a <> _ =  a

    
newtype AccmulateRight a b = AccmulateRight ( Validation a b ) deriving (Eq, Show)

instance (Semigroup a , Semigroup b)=> Semigroup (AccmulateRight a b ) where
    AccmulateRight (Success a) <> AccmulateRight (Success b)  = AccmulateRight(Success (a <> b))
    _ <> AccmulateRight (Failure a1) = AccmulateRight (Failure a1) 
    AccmulateRight (Failure a1) <> _ = AccmulateRight(Failure a1) 


newtype AccmulateBoth a b = AccmulateBoth( Validation a b) deriving (Eq, Show) 

instance (Semigroup a , Semigroup b)=> Semigroup (AccmulateBoth a b ) where
    AccmulateBoth (Success a) <> AccmulateBoth (Success b)  = AccmulateBoth(Success (a <> b))
    AccmulateBoth (Failure a) <> AccmulateBoth (Failure b)  = AccmulateBoth(Failure (a <> b))
    _ <> AccmulateBoth (Failure a1) = AccmulateBoth(Failure a1) 
    AccmulateBoth (Failure a1) <> _ = AccmulateBoth(Failure a1) 
    
    
