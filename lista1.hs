	import Data.Char
	type Desenho = [[Char]]

-- 4.27
	diagonalBlack :: Int -> Int -> [Char]
	diagonalBlack n m 
		| n <= 0 = ""
		| otherwise = linha ++ diagonalBlack ant prox 
		where
			ant = n-1
			prox = m+1
			linha = insereBranco(m) ++ inserePreto(1) ++ insereBranco( n-1 ) ++ "\n"

	insereBranco :: Int -> [Char]
	insereBranco n
		| n < 1 = ""
		| otherwise = "-" ++ insereBranco(n-1)

	inserePreto :: Int -> [Char]
	inserePreto n
		| n < 1 = ""
		| otherwise = "#" ++ inserePreto(n-1)

	geraDiagonalBlack :: Int -> Desenho
	geraDiagonalBlack n = words(diagonalBlack n 0)

	showDiagonalBlack :: Int -> IO()
	showDiagonalBlack = putStr . concat . map (++"\n") . geraDiagonalBlack



-- 4.29
	bothDiagonalBlack :: Int -> Int -> [Char]
	bothDiagonalBlack n m 
		| n <= 0 = ""
		| otherwise = linha ++ bothDiagonalBlack ant prox
		where
			ant = n-2
			prox = m+1
			linha = insereBranco(m) ++ inserePreto(1) ++ insereBranco( n-2 ) ++ inserePreto(1) ++ insereBranco(m) ++ "\n"

	geraBothDiagonalBlack :: Int -> Desenho
	geraBothDiagonalBlack n = (words(bothDiagonalBlack n 0)) ++ reverse (words(bothDiagonalBlack n 0))

	showBothDiagonalBlack :: Int -> IO()
	showBothDiagonalBlack = putStr . concat . map (++"\n") . geraBothDiagonalBlack



-- 4.32
	potencia2aN :: Integer -> Integer
	potencia2aN n
		| n == 0       = 1
	  | n == 1       = 2
	  | mod n 2 == 0 = (potencia2aN (div n 2)) ^ 2
	  | otherwise    = (potencia2aN (div (n-1) 2)) ^ 2 * 2



-- 5.19
	capitalizeLetters :: String -> String
	capitalizeLetters [] = []
	capitalizeLetters (a:x)
		| (ord a) >= 65 && (ord a) <= 90  = a : capitalizeLetters x
		| (ord a) >= 97 && (ord a) <= 122 = chr (ord a - 32) : capitalizeLetters x
		| otherwise												= capitalizeLetters x



-- 5.22
	onSeparateLines :: [String] -> String
	onSeparateLines [] = []
	onSeparateLines (a:x) = a ++ "\n" ++ (onSeparateLines x)

	showOnSeparateLines :: [String] -> IO()
	showOnSeparateLines = putStr . onSeparateLines


