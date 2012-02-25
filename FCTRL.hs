-- spoj FCTRL
-- `getLine` will cause TLE

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

z :: Int -> Int
z 0 = 0
z n = x + z x
    where x = n `div` 5

readInt :: BS.ByteString -> Int
readInt x =
    case BS.readInt x of
         Just (i,_) -> i
         Nothing -> error "Unparsable Int"

main = do
    (l:ls) <- BS.lines `fmap` BS.getContents
    let testcase = readInt l
    mapM_ (print . z . readInt) $ take testcase ls
