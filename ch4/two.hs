third_a :: [a] -> a
third_a l = head (tail (tail l))

third_b :: [a] -> a
third_b l = l !! 2

third_c :: [a] -> a
third_c (_:_:a:_) = a
