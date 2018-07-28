{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | https://stackoverflow.com/q/51513731
module Refine where

import Collection
import Data.Tagged

data PredicateKind = Mono | Hetero

type family Predicate (k :: PredicateKind) (i :: *) (r :: *) = t | t -> i r k
  where Predicate Mono   i i = i -> Bool
        Predicate Hetero i r = i -> Maybe r
        -- TODO: List membership.
        -- TODO: Open up and have a class. Or remove ~ from Ref instances.

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

newtype Ref φ p a = Ref (p -> Maybe a)  -- I do want this to have a switch of PredicateKind.

instance a ~ (Predicate k i r) => Functor (Ref φ a)  -- TODO: Derive?
  where
    fmap f (Ref c) = Ref $ \p -> fmap f (c p)

instance a ~ (Predicate k i r) => Applicative (Ref φ a)
  where
    pure x = Ref $ \_ -> pure x
    (Ref f) <*> (Ref c) = Ref $ \p -> f p <*> c p

-- instance a ~ (Predicate k i r) => Monad (Ref φ a)
--   where
--     (Ref c) >>= f = f c  -- If f gives not, I should give not.
--     Not >>= _ = Not

refine :: MakePredicate (Predicate k i r) i r => i -> Ref φ (Predicate k i r) r
refine x = Ref $ \p -> predicate p x

runRef :: Predicate k i r -> Ref φ (Predicate k i r) a -> Maybe a
runRef p (Ref f) = f p

-- λ runRef even (pure 2)
-- Just 2
-- λ runRef even (pure 3)
-- Nothing

