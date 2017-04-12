module Optional where
import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Only b) = Only (a `mappend` b)
    mappend _ (Only a) = Only a
    mappend (Only a) _ = Only a
