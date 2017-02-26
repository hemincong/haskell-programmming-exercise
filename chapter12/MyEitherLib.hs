module MyEitherLib where
  
data Either1 a b = Left1 a | Right1 b
  
lefts' :: [Either1 a b] -> [a]
lefts' [] = []
lefts' ((Left1 a):xs) = a : lefts' xs
lefts' ((Right1 a):xs) = lefts' xs

lefts2 :: [Either1 a b] -> [a]
lefts2 = foldr (\x b -> case x of 
                      (Left1 a1) -> a1:b
                      (Right1 b1) -> b) []
                      
rights' :: [Either1 a b] -> [b]
rights' [] = []
rights' ((Left1 _):xs) = rights' xs
rights' ((Right1 b1):xs) = b1:rights' xs
                      
partitionEithers' :: [Either1 a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b->c) -> Either1 a b -> Maybe c
eitherMaybe' _ (Left1 _) = Nothing
eitherMaybe' f (Right1 b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either1 a b -> c
either' fl _ (Left1 a) = fl a
either' _ fr (Right1 b) = fr b
                      

eitherMaybe'' :: (b->c)->Either1 a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\b -> Just (f b))
                      
