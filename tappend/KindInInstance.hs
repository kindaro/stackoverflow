{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module KindInInstance where

import GHC.TypeNats

class C (n :: Nat)

instance C 0
