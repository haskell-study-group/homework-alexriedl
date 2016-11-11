(&&) :: Bool -> Bool -> Bool
(&&) a b = do
    if a then
        if b then True
        else False
    else False
