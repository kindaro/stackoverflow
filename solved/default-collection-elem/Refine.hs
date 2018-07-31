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
{-# LANGUAGE GADTs #-}

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

