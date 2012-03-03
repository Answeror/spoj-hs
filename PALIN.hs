-- SPOJ PALIN
-- To find the next palindrome number large than K. K is not more than 10^6
-- digits.

import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main = do
    input <- BS.getContents
    mapM_ (BS.putStrLn . solve) (tail $ BS.lines input)

solve :: BS.ByteString -> BS.ByteString
solve ns = if (BS.reverse hs) > ls then make hs
                                   else make (inc hs)
    where
        (hs,ls) = if even (BS.length ns)
                     then (BS.take half ns, BS.drop half ns)
                     else (BS.take (half+1) ns, BS.drop half ns)
        half = (BS.length ns) `div` 2
        make s = s `BS.append` (BS.drop (BS.length s-half) (BS.reverse s))

inc :: BS.ByteString -> BS.ByteString
inc = BS.pack . fst . foldr f ([],1) . BS.unpack
    where f ch (s,c) = let x = ord ch - 48 + c
                           r = x `mod` 10
                           q = x `div` 10
                           in (chr (r + 48):s,q)
          g (s,c) = if c == 0 then s
                              else chr (c + 48):s
