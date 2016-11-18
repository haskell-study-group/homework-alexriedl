--One
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

--Two
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)
