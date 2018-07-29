{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Perhaps where

data Perhaps i r a
  where
    Actually :: a -> Perhaps i r a
    Sometimes :: i -> Perhaps i r r  -- Waiting to be converted to r.
    Never :: Perhaps i r a
    Fmap :: (a' -> a) -> Perhaps i r a' -> Perhaps i r a

instance Functor (Perhaps i r)
  where
    fmap f (Actually x) = Fmap f (Actually x)

runPerhaps :: (i -> Maybe r) -> Perhaps i r a -> Maybe a
runPerhaps p (Fmap f (Actually x)) = runPerhaps p $ Actually (f x)
runPerhaps _ (Actually x) = Just x
runPerhaps p (Sometimes i) = p i

-- |
-- λ runPerhaps undefined $ fmap show $ Actually 3
-- Just "3"
--
-- λ runPerhaps (\x -> if even x then Just x else Nothing) $ Sometimes 2
-- Just 2
-- λ runPerhaps (\x -> if even x then Just x else Nothing) $ Sometimes 3
-- Nothing
