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

-- ^
-- λ apfend (show @Int) (show @Char) (1 :: Int) 'a'  :: String
-- "1'a'"
