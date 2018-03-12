module SplitSubstr where

-- stackoverflow.com/questions/49228467

import System.Environment (getArgs)

import Data.Maybe

import Data.Algorithms.KMP

main :: IO ()
main = do
    (body: delims) <- getArgs

    print $ run body delims

run :: Eq a => [a] -> [[a]] -> [Int]
run body delims = catMaybes $ listToMaybe . flip match body . build <$> delims


