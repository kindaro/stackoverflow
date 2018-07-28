{-# LANGUAGE ExistentialQuantification #-}

module Perhaps where

data Perhaps i r a = Actually a
                   | Sometimes (i -> r) a
                   | Never
                   | forall a'. Fmap (a' -> a) (Perhaps i r a')

instance Functor (Perhaps i r)
  where
    fmap f (Actually x) = Fmap f (Actually x)

runPerhaps :: Perhaps i r a -> a
runPerhaps (Fmap f (Actually x)) = runPerhaps $ Actually (f x)
runPerhaps (Actually x) = x
