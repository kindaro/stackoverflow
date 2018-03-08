module StateSort where

-- stackoverflow.com/q/49164810

import Control.Monad.State
import System.Environment

myFunct' :: Ord a => [a] -> ([a], a)
myFunct' xs = (reverse xs, minimum xs)

-- λ myFunct' [1,2,3]
-- ([3,2,1],1)

stateExample1 :: State Integer Integer
stateExample1 = do
    x <- get
    return $ x * 2  -- (* 2) is an example of a pure function.

-- λ runState stateExample1 10
-- (20,10)  -- The first is the return value, the second is the (unchanged) state.

stateExample2 :: State Integer Integer
stateExample2 = do
    x <- get
    put $ x * 2  -- You may think of it as though it were "x = x * 2" 
                 -- in an imperative language.
    return 0

-- λ runState stateExample2 10
-- (0,20)  -- Now we have changed the state, and return 0.

myFunct :: Ord a => [a] -> State (Maybe a) [a]
myFunct [ ] = return [ ]
myFunct t = do
        let s = reverse t
        let a = minimum t
        put (Just a)
        return s

-- λ runState (myFunct [1,2,3]) (Just (-100))
-- ([3,2,1],Just 1)
-- λ runState (myFunct []) (Just (-100))
-- ([],Just (-100))

-- λ myFunct' []
-- ([],*** Exception: Prelude.minimum: empty list
-- λ runState (myFunct []) Nothing
-- ([],Nothing)

reverseAndMinimum :: Ord a => [a] -> ([a], Maybe a)
reverseAndMinimum xs = let (reversed', smallest) = runState (reverseAndMinimum' xs) Nothing
                       in  (reversed' [ ], smallest)

reverseAndMinimum' :: Ord a => [a] -> State (Maybe a) ([a] -> [a])
reverseAndMinimum' [ ] = return id
reverseAndMinimum' (x:xs) = do
    smallestSoFar <- get
    case smallestSoFar of
        Nothing -> put $ Just x
        Just y  -> when
                    (x < y)
                    (put $ Just x)
    fmap (. (x:)) (reverseAndMinimum' xs)

-- λ reverseAndMinimum [2,1,2,3]
-- ([3,2,1,2],Just 1)

main = do
    top <- (read :: String -> Int) . (!! 0) <$> getArgs
    print $ last . fst $ reverseAndMinimum [1..top]
