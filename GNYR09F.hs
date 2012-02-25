-- @title spoj GNYR09F
-- DP
-- 要注意被memo的函数和它的table要在同一个scope里

import Data.Array

solve bitcount adjcount = g bitcount adjcount 0 + g bitcount adjcount 1
    where
        g bitcount adjcount highbit
            | bitcount > adjcount = table ! (bitcount, adjcount, highbit)
            | otherwise = 0
        table :: Array (Int, Int, Int) Int
        table = listArray ((1, 0, 0), (100, 100, 1)) ls
        ls = [f i j k | i <- [1..100], j <- [0..100], k <- [0..1]]
        f 1 0 _ = 1
        f bitcount adjcount 0 = highzero + highone
            where highzero = g (bitcount - 1) adjcount 0
                  highone = g (bitcount - 1) adjcount 1
        f bitcount adjcount 1
            | adjcount == 0 = highzero
            | otherwise = highzero + highone
            where highzero = g (bitcount - 1) adjcount 0
                  highone = g (bitcount - 1) (adjcount - 1) 1

main = do
    input <- getLine
    let testcount = read input
    sequence . take testcount . repeat $ do
        input <- getLine
        let [testcase,bitcount,adjcount] = map read (words input)
        putStrLn (show testcase ++ " " ++ show (solve bitcount adjcount))
