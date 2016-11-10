halve :: [a] -> ([a], [a])
halve s = do
    let m = length s `div` 2
    (take m s, drop m s)
