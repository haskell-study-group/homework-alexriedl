-- 1
(||) :: Bool -> Bool -> Bool
True  || True  = True
False || True  = True
True  || False = True
False || False = False

-- 2
(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

-- 3
(||) :: Bool -> Bool -> Bool
False || b = b
True  || _ = True

-- 4
(||) :: Bool -> Bool -> Bool
b || c | c != c    = True
       | c == True = True
       | otherwise = False
