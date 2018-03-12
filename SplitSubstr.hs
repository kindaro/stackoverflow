module SplitSubstr where

-- stackoverflow.com/questions/49228467

import System.Environment (getArgs)

import Data.List
import Data.Maybe

import Data.Algorithms.KMP
import Data.List.Ordered

-- main :: IO ()
-- main = do
--     (body: delims) <- getArgs

    -- print $ run body delims

streams :: String -> [String] -> [(String, [Int])]
streams body delims = zip delims $ flip match body . build <$> delims

lengthDecoratedStreams :: [(String, [Int])] -> [[(Int, Int)]]
lengthDecoratedStreams = fmap (\ (delim, stream) -> zip stream (repeat $ length delim))

mergeStreams :: [[(Int, Int)]] -> [(Int, Int)]
mergeStreams streams = foldr merge [] streams

stream delims = mergeStreams . lengthDecoratedStreams . flip streams delims

splitOnSubstrs :: [String] -> String -> [String]
splitOnSubstrs delims body = unfoldr f body
  where
    f [ ]  = Nothing
    f body = case listToMaybe (stream delims body) of
        Just (start, length) ->
            let (word, rest) = splitAt start body
                body' = drop length rest
            in  return (word, body')
        Nothing -> return (body, [ ])

-- λ take 10 $ splitOnSubstrs ["||", "***"] "la||la***fa"
-- ["la","la","fa"]



-- run :: Eq a => String -> [String] -> [Int]
-- run body delims = filter (snd /= Nothing) $ zip ... listToMaybe . flip match body . build <$> delims


