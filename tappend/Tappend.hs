{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Tappend where

type family Concat (ts :: [*]) (ts' :: [*])
  where
    Concat '[ ] ts = ts
    Concat (t ': ts) ts' = t ': Concat ts ts'

type family F (ts :: [*]) a
  where
    F '[ ] a = a
    F (t ': ts) a = t -> F ts a

(...) :: F ts m -> F ts' m -> F (Concat ts ts') m
(...) f g = undefined
