{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Perhaps where

import Control.Monad ((>=>))
import Data.Tagged

-- $setup
-- λ import GHC.Natural

data Perhaps i r a
  where
    Actually :: a -> Perhaps i r a
    Possibly :: i -> (r -> a) -> Perhaps i r a
    Never :: Perhaps i r a
    Fmap :: (a' -> a) -> Perhaps i r a' -> Perhaps i r a
    Ap :: Perhaps i r (a' -> a) -> Perhaps i r a' -> Perhaps i r a
    Bind :: (a' -> Perhaps i r a) -> Perhaps i r a' -> Perhaps i r a

instance Functor (Perhaps i r)
  where
    fmap f x = Fmap f x

instance Applicative (Perhaps i r)
  where
    pure x = Actually x
    f <*> x = Ap f x

instance Monad (Perhaps i r)
  where
    x >>= f = Bind f x

runPerhaps :: (i -> Maybe r) -> Perhaps i r a -> Maybe a
runPerhaps _ (Actually x) = Just x
runPerhaps p (Possibly i f) = fmap f $ p i
runPerhaps _ Never = Nothing
runPerhaps p (Fmap f x) = fmap f (runPerhaps p x)
runPerhaps p (Ap f x) = runPerhaps p f <*> runPerhaps p x
runPerhaps p (Bind f x) = runPerhaps p =<< f <$> runPerhaps p x

-- |
-- λ runPerhaps undefined $ fmap show $ Actually 3
-- Just "3"
--
-- λ runPerhaps (\x -> if even x then Just x else Nothing) $ Possibly 2 id
-- Just 2
-- λ runPerhaps (\x -> if even x then Just x else Nothing) $ Possibly 3 id
-- Nothing

type Refined i r a = Tagged (i, r) a

attempt :: i -> Perhaps i r r
attempt x = Possibly x id

refine :: i -> Perhaps i r (Refined i r i)
refine x = Possibly x (const $ Tagged x)

release :: Refined i r a -> a
release = untag

apply :: Refined i r i -> Perhaps i r r
apply (Tagged x) = Possibly x id

obtain :: i -> Perhaps i r i
obtain = fmap release . refine

example p x y = runPerhaps p $ do
    x' <- attempt x
    y' <- attempt y
    return $ x' + y'

-- |
-- λ example (\x -> if even x then Just x else Nothing) 2 4
-- Just 6
-- λ example (\x -> if even x then Just x else Nothing) 2 3
-- Nothing
-- λ example (\x -> if x > 0 then Just x else Nothing) 2 3
-- Just 5
-- λ example (\x -> if x > 0 then Just x else Nothing) 2 0
-- Nothing
-- λ example (\x -> if x > 0 then (Just . fromInteger) x else Nothing) 2 3 :: Maybe Natural
-- Just 5
-- λ example (\x -> if x > 0 then (Just . fromInteger) x else Nothing) 2 (-1) :: Maybe Natural
-- Nothing
