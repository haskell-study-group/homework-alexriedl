--one :: (a -> b) (a -> Bool) -> [a] -> [a]
one m f (x) = Prelude.map m (filter f x)

--Two
all :: (a -> Bool) -> [a] -> Bool
all f [x] = f x
all f (s:xs) = (Main.all f xs) && f s

any :: (a -> Bool) -> [a] -> Bool
any f [x] = f x
any f (s:xs) = (Main.any f xs) || f s

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs) = do
    if f x
       then (x:Main.takeWhile f xs)
       else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs) = do
    if f x
       then (Main.dropWhile f xs)
       else (x:xs)

--Three
map :: (a -> b) -> [a] -> [b]
map f (x) = foldr (\a ax -> (f a:ax)) [] x

--Four
dec2int :: [Int] -> Int
dec2int [x] = x
dec2int (x:xs) = foldl (\a1 a2 -> a1 * 10 + a2) x xs

--Five

--Six

--Seven

--Eight

--Nine

--Ten
