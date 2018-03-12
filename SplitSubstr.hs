module SplitSubstr where

-- stackoverflow.com/questions/49228467

import System.Environment (getArgs)

import Data.List (unfoldr, isPrefixOf, elemIndex)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, isNothing)

-- main :: IO ()
-- main = do
--     (body: delims) <- getArgs

    -- print $ run body delims

splitOnSubstrs :: [String] -> String -> [String]
splitOnSubstrs delims
    = fmap catMaybes       -- At this point, there will be only `Just` elements left.
    . splitWhen isNothing  -- Now we may split at nothings.
    . unfoldr f            -- Replace the occurences of delimiters with a `Nothing`.
  where

-- | This is the base case. It will terminate the `unfoldr` process.
    f [ ]  = Nothing

-- | This is the recursive case. It is divided into 2 cases:
-- * One of the delimiters may match. We will then replace it with a Nothing.
-- * Otherwise, we will `Just` return the current element.
--
-- Notice that, if there are several patterns that match at this point, we will use the first one.
-- You may sort the patterns by length to always match the longest or the shortest. If you desire
-- more complicated behaviour, you must plug a more involved logic here. In any way, the index
-- should point to one of the patterns that matched.
--
--                       vvvvvvvvvvvvvv
    f body@(x:xs) = case elemIndex True $ (`isPrefixOf` body) <$> delims of
        Just index -> return (Nothing, drop (length $ delims !! index) body)
        Nothing    -> return (Just x, xs)

-- λ take 10 $ splitOnSubstrs ["||", "***"] "la||la***fa"
-- ["la","la","fa"]



-- run :: Eq a => String -> [String] -> [Int]
-- run body delims = filter (snd /= Nothing) $ zip ... listToMaybe . flip match body . build <$> delims


