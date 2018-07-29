{-# LANGUAGE DeriveFunctor #-}

-- | https://stackoverflow.com/q/51577631
module Yoneda where

newtype F a = F a deriving (Show, Functor)

type G f a = forall a'. (a -> a') -> f a'

fw x = x id

bw :: Functor f => f a -> G f a
bw x = \f -> fmap f x
