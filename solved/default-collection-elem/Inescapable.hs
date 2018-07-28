{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Inescapable where

data Φ

data I φ a = I a deriving (Show, Functor)

instance Applicative (I φ)
  where
    pure x = I x
    (I f) <*> (I x) = I (f x)

instance Monad (I φ)
  where
    (I x) >>= f = f x

runI :: (forall φ. I φ a) -> a
runI (I x) = x

data S φ a = S a deriving Show

newSym :: a -> I φ (S φ a)
newSym = I . S

readSym :: S φ a -> I φ a
readSym (S x) = I x
