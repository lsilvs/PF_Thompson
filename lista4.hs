import Test.QuickCheck
fac n = if n == 0 then 1 else n * fac (n-1)


-- 9.5
prop_Sum :: [Integer] -> [Integer] -> Bool
prop_Sum xs ys =
    sum (xs ++ ys) == sum xs + sum ys


-- 9.8
prop_Elem :: Integer -> [Integer] -> [Integer] -> Bool
prop_Elem n xs ys =
    elem n (xs ++ ys) == elem n xs || elem n ys


-- 9.13
facAux :: Integer -> Integer -> Integer
facAux 0 p = p
facAux n p = facAux (n-1) (n*p)

fac2 n = facAux n 1

prop_Fac :: Integer -> Bool
prop_Fac n =
    fac n == fac2 n
