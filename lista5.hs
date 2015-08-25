-- 10.13
sumSqr :: Integer -> Integer
sumSqr x = foldr (\a b -> a+b) 0 (map (\e -> e*e) [0..x])


-- 10.14
sumSqrPositive :: [Integer] -> Integer
sumSqrPositive xs = foldr (\a b -> a+b) 0 (map (\e -> e*e) (filter (\x-> x > 0) xs))


-- 10.15
last' :: [a] -> a
last' l1 = fst (foldr (\x y -> if snd y 
														  then (x, False) 
														  else y) (undefined, True) l1)

init' :: [a] -> [a]
init' l1 = fst (foldr (\x y -> if snd y then ([], False) else (x:(fst y), False)) (undefined, True) l1)


--unzip' :: [(a, b)] -> ([a], [b])
unzip' l1 = foldr unzipAux ([],[]) l1
unzipAux (a,b) (x,y) = (a:x,b:y)


-- 10.20
switchMap :: (Integer -> Integer) -> (Integer -> Integer) -> [Integer] -> [Integer]
switchMap f g l1 = map1 l1
	where
		map1 [] = []
		map1 (x:xs) = f x : map2 xs
		map2 [] = []
		map2 (x:xs) = g x : map1 xs

addOne :: Integer -> Integer
addOne n = n+1

addTen :: Integer -> Integer
addTen n = n+10


-- 10.21
split' :: [a] -> ([a], [a])
split' l1 = fst (foldl splitAux (([],[]), True) l1)

splitAux ((x,y), cond) b = if cond then ((x++[b],y), False) else ((x,y++[b]), True)


merge' :: ([a], [a]) -> [a]
merge' (l1, l2) = fst (foldl mergeAux ([],l2) l1)

mergeAux (x, [])  b = (x ++ [b], [])
mergeAux (x, a:y) b = (x ++ [b] ++ [a], y)


