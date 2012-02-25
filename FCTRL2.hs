main = do
    input <- getLine
    let testcase = read input
    sequence . take testcase . repeat $ do
        input <- getLine
        print $ f $ read input

f :: Integer -> Integer
f 0 = 1
f n = n * f (n - 1)
