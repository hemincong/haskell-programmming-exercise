module MyMaybeLib where
  
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False 

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee m _ Nothing = m
mayybee m f (Just n) = f n 

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just n) = n
fromMaybe n Nothing = n

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just n) = [n]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just n):xs) = n : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

ff (Just n) b = n : b

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:xs) = case flipMaybe xs of
                          Just ys -> Just (x:ys)
                          Nothing -> Nothing
            
