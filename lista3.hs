import Data.Char

-- 8.13
somaNnumeros :: IO ()
somaNnumeros = do
    putStrLn "Quantos numeros voce quer somar?"
    num <- getInt
    sum <- aux num
    print sum

aux :: Int -> IO Int
aux num = do
    putStrLn "Numero a ser somado:"
    n <- getInt
    if num == 1 then
        return n
    else do
        acum <- aux (num - 1)
        return (n + acum)

getInt :: IO Int
getInt = do str <- getLine
            return (read str)

-- 8.14
leTeclado :: IO String
leTeclado = do
      linha <- getLine
      if null linha
         then return ""
         else do
            acum <- leTeclado
            return (linha ++ "\n" ++ acum)

counter :: String -> (Int, Int, Int)
counter text = (length text, length (words text), length (lines text))

formatado :: (Int, Int, Int) -> String
formatado (c, w, l) = unlines $
     ["Numero de caracteres:"
     , show c
     , "Numero de palavras:"
     , show w
     , "Numero de linhas:"
     , show l
     ]

wc' = do
   texto <- leTeclado
   putStr (formatado (counter texto))

-- 8.15
palindromoCheck :: IO Bool
palindromoCheck = do
			    texto <- getLine
			    return (isPalindromo texto)

isPalindromo texto
 | tamanho <= 1          = tamanho == 1
 | primeiroNotLetter		 = isPalindromo (tail texto)
 | ultimoNotLetter		   = isPalindromo (init texto)
 | tamanho == 2          = primeiro == ultimo
 | primeiro /= ultimo    = False
 | primeiro == ultimo    = isPalindromo (take (tamanho - 2) (tail texto))
 where primeiro 				 = toLower (head texto)
       ultimo  					 = toLower (last texto)
       tamanho    			 = length texto
       primeiroNotLetter = (ord primeiro) < 79 || (ord primeiro) > 122
       ultimoNotLetter 	 = (ord ultimo) < 79 || (ord ultimo) > 122

--8.19
-- RESPOSTA: O programa apresentado copia todo texto de entrada e imprime na saída.
-- Ele usa a definição de um whileCopy que verifica se a linha digitada é vazia.
-- Caso seja, ele encerra a execução. Caso não seja, ele lê outra linha e chama o whileCopy novamente.
-- Após a definição do whileCopy usando o let, ele executa o whileCopy propriamente dito.











