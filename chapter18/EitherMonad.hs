module EitherMonad where

type Founded = Int
type Coders = Int

data SoftwareShop = 
    Shop {
        founded :: Founded
        , programmers :: Coders
    } deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFouned :: Int -> Either FoundedError Founded
validateFouned n 
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeYears n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFouned years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers


