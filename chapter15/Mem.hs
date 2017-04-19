module Main where

import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem ( \s -> (mempty, s) )
    mappend (Mem f) (Mem g) = Mem $ \x -> let (a, b) = g x 
                                              (c, d) = f x 
                                          in (a <> c, (snd (f . g $ x) ))
    
    --mappend (Mem f) (Mem g) = Mem $ \x -> let (a, b) = g x 
     --                                         (c, d) = f x 
      --                                    in (a <> c, snd $ f x) ) 
f' = Mem $ \s -> ("hi", s+1)

main :: IO()
main = do 
    print $ runMem (f' <> mempty ) 0 
    print $ runMem (mempty <> f') 0 
    print $ (runMem mempty  0 :: (String , Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0

