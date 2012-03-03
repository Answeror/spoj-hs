-- SPOJ PALIN
-- To find the next palindrome number large than K. K is not more than 10^6
-- digits.

import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main = do
    input <- BS.getContents
    mapM_ (BS.putStrLn . solve) (tail $ BS.lines input)

solve :: BS.ByteString -> BS.ByteString
solve input = let k = fst . fromJust $ BS.readInteger input
                  n = k+1
                  in next $ BS.pack $ show n

next :: BS.ByteString -> BS.ByteString
next n
    | even $ BS.length n = wheneven
    | otherwise = whenodd
    where
        wheneven :: BS.ByteString
        wheneven
            | (BS.reverse highs) >= lows = BS.concat [highs,BS.reverse highs]
            | BS.length inchighs == BS.length highs = BS.concat [inchighs,BS.reverse inchighs]
            | otherwise = BS.concat [inchighs,BS.tail (BS.reverse inchighs)]
        whenodd :: BS.ByteString
        whenodd
            | (BS.reverse highs) >= lows = BS.concat [highmids,BS.reverse highs]
            | BS.length inchighmids == BS.length highmids = BS.concat [inchighmids,BS.tail (BS.reverse inchighmids)]
            | otherwise = BS.concat [inchighmids,BS.tail (BS.tail (BS.reverse inchighmids))]
        (highs,mids,lows) = sp n
        highmids = BS.concat [highs,mids]
        inchighs = BS.pack $ show ((fst $ fromJust $ BS.readInteger highs)+1)
        inchighmids = BS.pack $ show ((fst $ fromJust $ BS.readInteger highmids)+1)
        sp :: BS.ByteString -> (BS.ByteString,BS.ByteString,BS.ByteString)
        sp n
            | even $ BS.length n = (BS.take half n,BS.pack [],BS.drop half n)
            | otherwise = (BS.take half n,BS.take 1 $ BS.drop half n,BS.drop (half+1) n)
            where half = (BS.length n) `div` 2
