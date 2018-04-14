module ParseError where

-- https://stackoverflow.com/q/49828678

import Data.Map (Map)
import Data.List (sort)
import qualified Data.Map as M

type Anagrams = Map String [String]

main :: IO()
main = buildAnagrams "dictionary.txt" >>= loop
  where
    loop :: Anagrams -> IO ()
    loop anagrams = forever $ do
         putStrLn "Anagrams of..."
         getLine >>=  print . lookupAnagrams anagrams


buildAnagrams :: FilePath -> IO Anagrams
buildAnagrams = fmap (foldr f M.empty . lines ) . readFile
  where 
     f :: String -> Anagrams -> Anagrams
     f s = M.insertWith (++) (sort s) [s]

lookupAnagrams :: Anagrams -> String -> [String]
lookupAnagrams anagrams str =   
    case M.Lookup(sort str) anagrams of
        Nothing -> []
        Just XS -> XS
