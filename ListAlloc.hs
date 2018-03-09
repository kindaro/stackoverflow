module ListAlloc where
-- stackoverflow.com/q/49185194

import Data.IntSet
main = do
   print $ size
         $ fromAscList
         $ [1..1000]
