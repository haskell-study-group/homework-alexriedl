safetail_a :: [a] -> [a]
safetail_a l = do
    if null l
    then l
    else tail l

safetail_b :: [a] -> [a]
safetail_b l | null l = l
             | otherwise = tail l

safetail_c :: [a] -> [a]
safetail_c (_:l) = l
safetail_c _ = []
