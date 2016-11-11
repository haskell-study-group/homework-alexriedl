grid :: Int -> Int -> [(Int,Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y], a /= b]

square :: Int -> [(Int,Int)]
square m = grid m m
