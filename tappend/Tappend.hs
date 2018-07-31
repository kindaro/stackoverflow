{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Tappend where

type family (ts :: [*]) ++ (ts' :: [*])
  where
    '[ ] ++ ts = ts
    (t ': ts) ++ ts' = t ': ts ++ ts'

type family F (ts :: [*]) a = r | r a -> ts
  where
    F '[ ] a = a
    F (t ': ts) a = t -> F ts a

(...) :: F ts m -> F ts' m -> F (ts ++ ts') m
(...) f g = undefined
