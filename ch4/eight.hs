luhnDouble :: Int -> Int
luhnDouble a = do
    if a > 4
       then a * 2 - 9
       else a * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = do
    mod ((luhnDouble a) + b + (luhnDouble c) + d) 10 == 0
