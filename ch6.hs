--One
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

--Two
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

--Three
(^) :: Int -> Int -> Int
m ^ 1 = m
m ^ n = m * (m * (n - 1))

--Four
euclid :: Int -> Int -> Int
euclid m n
    | m == n = m
    | m > n = euclid (m-n) n
    | n > m = euclid (n-m) m

--Five

--Six
and :: [Bool] -> Bool
and [] = True
and (False:xs) = False
and (True:xs) = Main.and xs

--concat :: [[a]] -> [a]

replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a = (a : (Main.replicate (n-1) a))

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = xs Main.!! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) = a == x || Main.elem a xs

--Seven
merge :: Ord a => [a] -> [a] -> [a]
merge (x) [] = x
merge [] (x) = x
merge (x:xs) (a:as)
    | x < a =  (x: (Main.merge xs (a:as)))
    | x >= a = (a: (Main.merge (x:xs) as))

--Eight
halve :: [a] -> ([a],[a])
halve a = do
    let m = length a `div` 2
    (Prelude.take m a, drop m a)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x) = merge (msort (fst (halve x))) (msort (snd (halve x)))
    {-
msort (x) = do
    let h = halve x
    let a = fst h
    let b = snd h
    merge (msort a) (msort b)
    -}
   
--Nine
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + Main.sum xs

take :: Int -> [a] -> [a]
take 1 (x:xs) = [x]
take n (x:xs) = (x:Main.take (n-1) xs)

last :: [a] -> a
last [x] = x
last (x:xs) = Main.last xs
