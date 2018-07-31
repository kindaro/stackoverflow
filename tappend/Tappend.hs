{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Tappend where

import Data.Monoid
import GHC.TypeNats
import Data.Vector.Sized

-- $setup
-- λ :set -XTypeApplications

class Apfend a b c
  where
    apfend :: a -> b -> c

instance Monoid m => Apfend (a -> m) (b -> m) (a -> b -> m)
  where
    apfend f g = \x y -> f x `mappend` g y

instance Apfend a b c => Apfend (d -> a) b (d -> c)
  where
    apfend f g = \x -> f x `apfend` g

-- ^
-- λ apfend (\x y -> show @Int x <> show @Int y) (show @Char) (1 :: Int) (2 :: Int) 'a'  :: String
-- ...
-- ...error...
-- ...Overlapping instances...
-- ...
