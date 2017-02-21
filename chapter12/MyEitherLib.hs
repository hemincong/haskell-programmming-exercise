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
                      
partitionEithers' :: [Either1 a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' ((Left1 a):xs) = (a:(fst $ partitionEithers' xs), snd (partitionEithers' xs))
partitionEithers' ((Right1 b):xs) = ((fst $ partitionEithers' xs), (b:(snd $ partitionEithers' xs)))
                      


                      
