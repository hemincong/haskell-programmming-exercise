{-# LANGUAGE FlexibleInstances #-}

module Lib2 where

import Data.String

newtype Flip f a b = Flip ( f b a) deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f , Functor g)=> Functor (Parappa f g) where
    fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga) 


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g)=> Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb) 

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g)=> Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List1 a = Nil | Cons a (List1 a)

instance Functor List1 where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a )=  OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a)  (fmap f b) (fmap f c) 

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a)  = Print s (f a)
    fmap f (Read sa) = Read (fmap f sa)
