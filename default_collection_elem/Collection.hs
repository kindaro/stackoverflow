{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Collection where

import Data.Tagged

class Collected phantom
  where
    type Element phantom
    type Element phantom = Tagged phantom Int

    type Collection phantom = r | r -> phantom
    type Collection phantom = Tagged phantom [Int]

    collection :: Collection phantom

    inCollection :: Int -> Collection phantom -> Maybe (Element phantom)
    -- TODO: Remove the second argument, use the default collection automagically.

data Primes

instance Collected Primes
  where
    type Element Primes = Tagged Primes Int
    type Collection Primes = Tagged Primes [Int]

    collection = Tagged [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

    inCollection element collection
        | element `elem` unTagged collection = Just $ Tagged element
        | otherwise = Nothing

-- ^
-- λ inCollection 7 (collection :: Collection Primes)
-- Just (Tagged 7)
-- λ inCollection 8 (collection :: Collection Primes)
-- Nothing


