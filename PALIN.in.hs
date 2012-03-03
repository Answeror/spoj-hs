main = do
    print 1
    putStrLn $ '1':(take 999999 (repeat '2'))
