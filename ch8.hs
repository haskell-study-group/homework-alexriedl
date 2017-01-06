--One
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

--Three
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

count :: Tree a -> Int
count (Leaf _) = 1
count (Node x y) = count x + count y

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = count r - count l <= 1 && (balanced l) && (balanced r)

--Five
data Expr = Val Int | Add Expr Expr deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add a b) = g (folde f g a) (folde f g b)

--Six
eval :: Expr -> Int
eval (Val a) = a
eval (Add a b) = (eval a) + (eval b)

eval2 :: Expr -> Int
eval2 = folde (\x -> x) (+)

size :: Expr -> Int
size (Val _) = 1
size (Add a b) = size a + size b

--size2 :: Expr -> Int
--size2 = fold (

--Seven
data Optional a = None | Some a
data List a = Empty | Cons a (List a)

instance Eq a => Eq (Optional a) where
    None == None = True
    (Some a) == (Some b) = a == b
    _ == _ = False

{-|
instance Eq a => Eq List a where
    Empty == Empty = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _ == _ = False
-}


--Eight
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

evalProp :: Subst -> Prop -> Bool
evalProp _ (Const b)    = b
evalProp s (Var x)      = find x s
evalProp s (Not p)      = not (evalProp s p)
evalProp s (And p q)    = evalProp s p && evalProp s q
evalProp s (Imply p q ) = evalProp s p <= evalProp s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

--Nine

