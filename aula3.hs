import Data.IntMap (insert)
{- Assunto: listas e tuplas -}

periodo::Int
periodo = 7

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise = n


-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

{- 01 função que retorna uma lista de vendas -}
--listaVendas :: Int-> [Int]

{- 02 função que retorna [[Int]] com listas de dia e venda -}

f2L::Int->[[Int]]
f2L 0 = []
f2L x = [x,vendas x]:f2L (x-1)  
----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}

insereL :: Int -> [Int] -> [Int]
insereL x []= [x]
insereL x (y:ys)
    | x >= y = y : insereL x ys
    | otherwise = x : insereL y ys 

ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista (x:xs) = insereL x (ordenaLista xs) 

-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}

insereListaLista::[Int]-> [[Int]] -> [[Int]]
insereListaLista x [] = [x]
insereListaLista (x:xs) ((z:zs):ys)
    | x <= z = (x:xs) : (z:zs) : ys
    | otherwise = (z:zs) : insereListaLista (x:xs) ys

ordenaListaLista :: [[Int]] -> [[Int]]
ordenaListaLista [] = []
ordenaListaLista (x:xs) = insereListaLista x (ordenaListaLista xs) 

---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
--ordenaLILE::[[Int]] ->[[Int]]

ordenaLILE::[[Int]]->[[Int]]
ordenaLILE [x] = [x]
ordenaLILE (x:xs) = ordenaListaLista (ordenaLista x : ordenaLILE xs)

 

-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
f6T :: Int-> [(Int, Int)]
f6T 0 = []
f6T x = (x,vendas x):f6T (x-1)

{- 07 função que gera o total de vendas-}

totalVendasT :: [(Int, Int)] -> Int
totalVendasT [] = 0
totalVendasT ((dia, venda):xs) = venda + totalVendasT xs

vendaParametro :: Int -> Int
vendaParametro 0 = 0
vendaParametro x = totalVendasT (f6T x)


{- 08 função que retorna a maior venda -} 
maiorVendasT8a::[(Int, Int)] -> Int 
maiorVendasT8a  [(dia,venda)] = venda
maiorVendasT8a ((dia,venda):xs)
    | venda > maiorVendasT8a xs  = venda
    | otherwise = maiorVendasT8a xs

{- 08-b como implementar com apenas os parâmetros? -}
maiorVendaT8b::[(Int, Int)] -> Int    
maiorVendaT8b [] = 0
maiorVendaT8b ((a,b):c) = maxi b (maiorVendaT8b c)

maiorVendaT8c::[(Int, Int)] -> Int    
maiorVendaT8c [] = 0
maiorVendaT8c (a:c) = maxi (snd a) (maiorVendaT8c c)

maiorVendaT8d::[(Int, Int)] -> Int    
maiorVendaT8d [] = 0
maiorVendaT8d c = maxi (snd(head c)) (maiorVendaT8d (tail c))
  
