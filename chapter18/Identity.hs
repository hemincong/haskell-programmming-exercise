module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure x = Identity x
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do 
        a <- arbitrary
        return (Identity a)

instance Eq a => EqProp (Identity a) where (=-=) = eq

functorTrigger = quickBatch ( functor ( undefined :: Identity (Int, String, Int)))
applicativeTrigger = quickBatch ( applicative ( undefined :: Identity (Int, String, Int)))
monadTrigger = quickBatch ( monad ( undefined :: Identity (Int, String, Int)))

