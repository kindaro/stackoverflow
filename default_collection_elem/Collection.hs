{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}

module Collection where

import Data.Tagged

-- $setup
--
-- λ :set -XTypeApplications

class Collected phantom
  where
    type Element phantom = r | r -> phantom
    type Element phantom = Tagged phantom Int

    type Collection phantom = r | r -> phantom
    type Collection phantom = Tagged phantom [Int]

    collection :: Collection phantom

    inCollection :: Int -> Maybe (Element phantom)

data Primes

instance Collected Primes
  where
    type Element Primes = Tagged Primes Int
    type Collection Primes = Tagged Primes [Int]

    collection = Tagged [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

    -- inCollection :: Int -> Maybe (Element Primes)
    inCollection element
        | element `elem` unTagged (collection @Primes) = Just $ Tagged element
        | otherwise = Nothing

-- ^
-- λ inCollection @Primes 7
-- Just (Tagged 7)
-- λ inCollection @Primes 8
-- Nothing

