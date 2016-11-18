--One

--one :: (a -> b) (a -> Bool) -> [a] -> [a]
one m f (x) = map m (filter f x)
