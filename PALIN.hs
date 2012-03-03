-- |SPOJ PALIN
-- To find the next palindrome number large than K. K is not more than 10^6
-- digits. 
-- Note:
-- 1. use ByteString
-- 2. not use Integer
-- 3. write inc manually
-- The third point is the most important. Do not use carried arithmetic,
-- instead, find the last number which is not '9'.

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
        make s = s ~~ (BS.drop (BS.length s-half) (BS.reverse s))

inc :: BS.ByteString -> BS.ByteString
inc = f . BS.spanEnd (=='9')
    where f (high,low) = let high' = if BS.null high
                                        then BS.pack "1"
                                        else BS.snoc (BS.init high)
                                             (chr (1 + ord (BS.last high)))
                            in high' ~~ (BS.replicate (BS.length low) '0')

(~~) = BS.append
