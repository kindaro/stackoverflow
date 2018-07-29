{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Perhaps where

import Control.Monad ((>=>))

-- $setup
-- λ import GHC.Natural

data Perhaps i r a
  where
    Actually :: a -> Perhaps i r a
    Sometimes :: i -> Perhaps i r r  -- Waiting to be converted to r.
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
runPerhaps p (Sometimes i) = p i
runPerhaps _ Never = Nothing
runPerhaps p (Fmap f x) = fmap f (runPerhaps p x)
runPerhaps p (Ap f x) = runPerhaps p f <*> runPerhaps p x
runPerhaps p (Bind f x) = runPerhaps p =<< f <$> runPerhaps p x

-- |
-- λ runPerhaps undefined $ fmap show $ Actually 3
-- Just "3"
--
-- λ runPerhaps (\x -> if even x then Just x else Nothing) $ Sometimes 2
-- Just 2
-- λ runPerhaps (\x -> if even x then Just x else Nothing) $ Sometimes 3
-- Nothing

data Presumably a = Presumably a deriving Show

assume :: i -> Perhaps i r (Presumably i)
assume = Actually . Presumably

verify :: Presumably i -> Perhaps i r r
verify (Presumably x) = Sometimes x

introduce :: i -> Perhaps i r r
introduce = assume >=> verify

example p x y = runPerhaps p $ do
    x' <- introduce x
    y' <- introduce y
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
