import Data.Array

isprime n = n > 1 && foldr f True primes
    where f i state = i*i > n || n `mod` i /= 0 && state

primes = 2:filter isprime [3,5..]

primesBetween m n
    | m > n = []
    | otherwise = eratos ps xs
    where ps = (takeWhile (\x -> x*x <= n) primes)
          xs = dropWhile (<2) [m..n]
          eratos (p:ps) xs = eratos ps (xs `minus` [first, first+p..n])
            where first
                    | p < m = p*((m+p-1) `div` p)
                    | otherwise = p + p
          eratos _ xs = xs

minus (x:xs) (y:ys) = case (compare x y) of
                           LT -> x:minus xs (y:ys)
                           EQ -> minus xs ys
                           GT -> minus (x:xs) ys
minus xs _ = xs

main = do
    input <- getLine
    let testcount = read input
    sequence . take testcount . repeat $ do
        input <- getLine
        let [m,n] = map read $ words input
        if n < 100000
            then mapM_ print (dropWhile (<m) $ takeWhile (<=n) primes)
            else mapM_ print (primesBetween m n)
        putStrLn ""
