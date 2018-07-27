{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | https://stackoverflow.com/a/51503924
module Collection where

import Data.Tagged

-- $setup
--
-- λ :set -XTypeApplications

class Collected phantom
  where
    type Carrier phantom

    type Index phantom

    type Element phantom = r | r -> phantom
    type Element phantom = Tagged phantom (Carrier phantom)

    type Collection phantom = r | r -> phantom
    type Collection phantom = Tagged phantom [Carrier phantom]

    collection :: Collection phantom

    inCollection :: Carrier phantom -> Maybe (Element phantom)
    default inCollection :: ( Element phantom    ~ Tagged phantom  (Carrier phantom)
                            , Collection phantom ~ Tagged phantom [Carrier phantom]
                            , Eq (Carrier phantom) )
                         => Carrier phantom -> Maybe (Element phantom)
    inCollection element
        | element `elem` unTagged (collection @phantom) = Just $ Tagged element
        | otherwise = Nothing

data Primes

instance Collected Primes
  where
    type Carrier Primes = Int
    type Index Primes = Int
    type Element Primes = Tagged Primes Int
    type Collection Primes = Tagged Primes [Int]

    collection = Tagged [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

-- ^
-- λ inCollection @Primes 7
-- Just (Tagged 7)
-- λ inCollection @Primes 8
-- Nothing

