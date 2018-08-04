{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}

-- | https://stackoverflow.com/q/51513731
module Refine where

import Collection
import Data.Tagged

import Data.Maybe (isJust)

-- $setup
-- λ :set -XFlexibleContexts
-- λ :set -XTypeApplications

-- | Various ways to obtain a partial function.

class Partial p i r | p -> i r
  where partial :: p -> i -> Maybe r
        default partial :: r ~ i => p -> i -> Maybe r
        partial p x | predicate p x = Just x | otherwise = Nothing
        predicate :: p -> i -> Bool
        predicate p x = isJust $ (partial :: p -> i -> Maybe r) p x

instance Partial (i -> Bool) i i
  where predicate p = p

-- ^
-- λ partial even 2 :: Maybe Integer
-- Just 2
-- λ partial even 3 :: Maybe Integer
-- Nothing

instance Partial (i -> Maybe r) i r
  where partial p = p

instance Eq i => Partial [i] i i
  where predicate p x = x `elem` p

-- ^
-- λ partial [2,3] 2 :: Maybe Integer
-- Just 2
-- λ partial [2,3] 4 :: Maybe Integer
-- Nothing

-- |
--   φ = phantom
--   i = initial
--   r = refined

data Ref φ i r a
  where
    Actually ::  a                                    -> Ref φ i r a
    Possibly ::  i -> (r -> a)                        -> Ref φ i r a
    Never    ::                                          Ref φ i r a

    Fmap     ::            (a' -> a) -> Ref φ i r a'  -> Ref φ i r a
    Ap       ::  Ref φ i r (a' -> a) -> Ref φ i r a'  -> Ref φ i r a
    Bind     ::  (a' -> Ref φ i r a) -> Ref φ i r a'  -> Ref φ i r a

instance Functor (Ref φ i r)
  where fmap f x = Fmap f x

instance Applicative (Ref φ i r)
  where pure x = Actually x
        f <*> x = Ap f x

instance Monad (Ref φ i r)
  where x >>= f = Bind f x

runRef :: Partial p i r => p -> Ref φ i r a -> Maybe a

runRef _ (Actually x) = Just x
runRef p (Possibly i f) = fmap f $ partial p i
runRef _ Never = Nothing

runRef p (Fmap f x) = fmap f (runRef p x)
runRef p (Ap f x) = runRef p f <*> runRef p x
runRef p (Bind f x) = runRef p =<< f <$> runRef p x

type Refined i r a = Tagged (i, r) a

attempt :: i -> Ref φ i r r
attempt x = Possibly x id

refine :: i -> Ref φ i r (Refined i r i)
refine x = Possibly x (const $ Tagged x)

release :: Refined i r a -> a
release = untag

apply :: Refined i r i -> Ref φ i r r
apply (Tagged x) = Possibly x id

obtain :: i -> Ref φ i r i
obtain = fmap release . refine

-- |
-- λ runRef even $ obtain 2
-- Just 2
-- λ runRef even $ obtain 3
-- Nothing
