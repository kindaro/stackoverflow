{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    default inCollection :: ( Element phantom    ~ Tagged phantom  Int
                            , Collection phantom ~ Tagged phantom [Int] )
                         => Int -> Maybe (Element phantom)
    inCollection element
        | element `elem` unTagged collection = Just $ Tagged element
        | otherwise = Nothing
      where
        collection = (collection :: Collection phantom)

data Primes

instance Collected Primes
  where
    type Element Primes = Tagged Primes Int
    type Collection Primes = Tagged Primes [Int]

    collection = Tagged [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

-- ^
-- λ inCollection @Primes 7
-- Just (Tagged 7)
-- λ inCollection @Primes 8
-- Nothing

