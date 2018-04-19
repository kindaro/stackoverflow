module ValueFromPrevious where

-- stackoverflow.com/q/49868650

double x = case x of
 []   -> []
 x:xs -> (if (x*2 < 10) then [x*2] else [x*2 `div` 10 + x*2 `mod` 10]) ++ double xs
