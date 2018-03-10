module ListAlloc where
-- stackoverflow.com/q/49185194

import Data.IntSet
main = do
   print $ size
         $ {-# SCC "fromDistinctAscList" #-} fromDistinctAscList
         $ {-# SCC "generate_list" #-} [1::Int .. 10^7]
