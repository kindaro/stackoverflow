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

-- $setup
-- λ :set -XFlexibleContexts

-- | Various ways to obtain a partial function.

class Partial p i r
  where partial :: p -> i -> Maybe r

instance Partial (i -> Bool) i i
  where partial p x | p x = Just x | otherwise = Nothing

-- ^
-- λ partial even 2 :: Maybe Integer
-- Just 2
-- λ partial even 3 :: Maybe Integer
-- Nothing

instance Partial (i -> Maybe r) i r
  where partial p x = p x

instance Eq i => Partial [i] i i
  where partial p x | x `elem` p = Just x | otherwise = Nothing

-- ^
-- λ partial [2,3] 2 :: Maybe Integer
-- Just 2
-- λ partial [2,3] 4 :: Maybe Integer
-- Nothing

-- |
--   φ = phantom
--   i = initial
--   r = refined

data Ref φ p a = Ref Bool (p -> Maybe a)  -- I do want this to have a switch of PredicateKind.

instance a ~ (Predicate k i r) => Functor (Ref φ a)  -- TODO: Derive?
  where
    fmap f (Ref b c) = Ref b $ \p -> fmap f (c p)

instance a ~ (Predicate k i r) => Applicative (Ref φ a)
  where
    pure x = Ref True $ \_ -> pure x
    (Ref b f) <*> (Ref b' c) = Ref (b && b') $ \p -> f p <*> c p

-- instance a ~ (Predicate k i r) => Monad (Ref φ a)
--   where
--     (Ref c) >>= f = f c  -- If f gives not, I should give not.
--     Not >>= _ = Not

refine :: MakePredicate (Predicate k i r) i r => i -> Ref φ (Predicate k i r) r
refine x = Ref True $ \p -> predicate p x

runRef :: Predicate k i r -> Ref φ (Predicate k i r) a -> Maybe a
runRef p (Ref b f) = f p

-- λ runRef even (pure 2)
-- Just 2
-- λ runRef even (pure 3)
-- Nothing

