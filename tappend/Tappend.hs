{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Tappend where

type family F (ts :: [*]) a
  where
    F '[ ] a = a
    F (t ': ts) a = t -> F ts a

(...) :: F '[b] c -> F '[a] b -> F '[a] c
(...) f g = f . g

-- |
-- λ show ... (+2) $ 3
-- "5"

