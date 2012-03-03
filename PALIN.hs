-- SPOJ PALIN
-- To find the next palindrome number large than K. K is not more than 10^6
-- digits.

import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main = do
    input <- BS.getContents
    mapM_ (BS.putStrLn . BS.pack . solve . BS.unpack) (tail $ BS.lines input)

solve = next . inc

inc = fst . foldr f ([],1)
    where f ch (s,c) = let x = ord ch - 48 + c
                           r = x `mod` 10
                           q = x `div` 10
                           in (chr (r + 48):s,q)
          g (s,c) = if c == 0 then s
                              else chr (c + 48):s

next ns = case (reverse hs) `compare` ls of
               EQ -> ns
               GT -> make hs
               LT -> make (inc hs)
    where
        ln = length ns
        half = ln `div` 2
        hs = take (ln - half) ns
        ls = drop half ns
        make s = s ++ (drop (length s-half) (reverse s))
