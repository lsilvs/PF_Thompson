-- 7.9
unique' :: [Integer] -> [Integer]
unique' [] = []
unique' (a:[]) = [a]
unique' (a:x)
	| naLista				= unique' [y | y <- x, y/=a]
	| otherwise			= a : unique' x
	where
		naLista = (length [y | y <- x, y==a]) > 0


-- 7.14
isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted (a:[]) = True
isSorted (a:b:x)
	| a < b 		= isSorted (b:x)
	| otherwise = False


-- 7.25
isSubList :: String -> String -> Bool
isSubList [] _ = True
isSubList _ [] = False
isSubList (a:x) (b:y)
	| a==b 			= isSubList x y
	| otherwise = isSubList (a:x) y

isSubSeq :: String -> String -> Bool
isSubSeq [] _ = True
isSubSeq _ [] = False
isSubSeq (a:x) (b:y)
	| a==b 			= x == (take (length x) y) || isSubSeq (a:x) y
	| otherwise = isSubSeq (a:x) y


-- 7.32
wc :: String -> (Int, Int, Int)
wc [] = (0,0,0)
wc text = (caracteres, palavras, linhas)
	where
		caracteres = length [x | x<-text, x/=' ' && x/='\n']
		palavras = 1 + length [x | x<-text, x==' ' || x=='\n']
		linhas = 1 + length [x | x<-text, x=='\n']


-- Problema da Celebridade
isCelebridade :: Ord a => [a] -> [a]
isCelebridade [] = []
isCelebridade (a:x)
	| ehConhecido && naoConhece = a : (isCelebridade x)
	| otherwise = isCelebridade x
	where
		ehConhecido = length [y | y<-x, y `conhece` a] == length x
		naoConhece 	= length [y | y<-x, a `conhece` y] == 0

-- IMPLEMENTAR A FUNCAO 'conhece' QUE RETORNA VERDADEIRO A QUANDO B CONHECE E FALSO QUANDO A NÃO CONHECE B (não sabia como fazer, então deixei uma função stub aqui)
conhece :: Ord a => a -> a -> Bool
conhece _ _ = True

