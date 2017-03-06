module GimmePerson where

type Name = String

type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

printYay :: (Either PersonInvalid Person) -> String
printYay p = case p of 
    (Right xx) -> "Yay! Successfully got a person:" ++ (show $ p )
    (Left xx1) -> show $ p
  
gimmePerson = do
  putStrLn $ "username:" 
  username <- getLine
  putStrLn $ "age:"
  age  <- getLine
  putStrLn $ printYay $ mkPerson username (read age::Integer)
  --putStrLn $ show $ mkPerson username (read age::Integer)
  return()
  
