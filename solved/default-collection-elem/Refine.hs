{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | https://stackoverflow.com/q/51513731
module Refine where

import Collection
import Data.Tagged

data PredicateKind = Mono | Hetero

type family Predicate (k :: PredicateKind) (i :: *) (r :: *) = t | t -> i r k
  where Predicate Mono   i i = i -> Bool
        Predicate Hetero i r = i -> Maybe r

class MakePredicate p i r
  where predicate :: p -> i -> Maybe r

instance MakePredicate (i -> Bool) i i
  where predicate p x | p x = Just x | otherwise = Nothing

instance MakePredicate (i -> Maybe r) i r
  where predicate p x = p x

-- |
--   φ = phantom
--   i = initial
--   r = refined

data Ref φ i r a = forall (p :: PredicateKind) . Ref (Predicate p i r -> a) | Not

instance Functor (Ref φ i r)  -- TODO: Derive?
  where
    fmap f (Ref c) = Ref $ \p -> f (c p)
    fmap _ Not = Not

instance Applicative (Ref φ i r)
  where
    pure x = Ref $ \_ -> x
    (Ref f) <*> (Ref c) = Ref $ \p -> (f p) (c _)  -- TODO: Needs injective type family!
    -- _ <*> Failure = Failure
    -- Failure <*> _ = Failure

-- instance Monad (M s)
--   where
--     (M x) >>= f = f x
--     Failure >>= _ = Failure

-- verify :: (Collected phantom, Carrier phantom ~ Int, Element phantom ~ Tagged phantom Int)
--        => Carrier phantom -> M Int (C phantom (Carrier phantom))
-- verify x = maybe Failure _ $ collected y
--   where collected :: Carrier phantom -> Maybe (Element phantom)
--         collected = inCollection
--         y :: Carrier phantom
--         y = x

-- runM :: M s a -> a
-- runM (M x) = x

-- λ runM (verify @Primes 3)
-- Tagged 3


