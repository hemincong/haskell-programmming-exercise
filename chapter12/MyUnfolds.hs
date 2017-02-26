module MyUnfolds where
  
 
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f $ f a)

myUnfoldr :: (b -> Maybe (a , b)) -> b -> [a]
myUnfoldr f b = case f b of
                Nothing -> []
                Just(a1, b1) -> a1 : myUnfoldr f b1
                
betterIterate :: (a->a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))