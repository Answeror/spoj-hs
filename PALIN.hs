-- SPOJ PALIN
-- To find the next palindrome number large than K. K is not more than 10^6
-- digits.

import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
    input <- BS.getContents
    mapM_ (BS.putStrLn . solve) (tail $ BS.lines input)

solve :: BS.ByteString -> BS.ByteString
solve = next . inc

inc :: BS.ByteString -> BS.ByteString
inc = BS.pack . show . (1+) . fst . fromJust . BS.readInteger

next :: BS.ByteString -> BS.ByteString
next ns = if BS.reverse hs >= ls
            then make hs
            else make (inc hs)
    where
        (hs,ls) = if even (BS.length ns)
                     then (BS.take half ns, BS.drop half ns)
                     else (BS.take (half+1) ns, BS.drop half ns)
        half = (BS.length ns) `div` 2
        make s = s `BS.append` (BS.drop (BS.length s-half) (BS.reverse s))

