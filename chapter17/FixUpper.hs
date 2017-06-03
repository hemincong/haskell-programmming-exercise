module FixUpper where


xx =  const <$> Just "hello" <*> pure "world"

x2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "tierness" <*> pure [1,2,4]
