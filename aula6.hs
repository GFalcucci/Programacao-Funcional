{- Objetivo da aula
   Exercícios e introdução à List Comprehension
-}

import Data.Char

------------------------------------------------------------
-- List Comprehension
------------------------------------------------------------

{- Multiplica todos os elementos pares da lista pela escalar -}
f1 :: Int -> [Int] -> [Int]
f1 z l = [a * z | a <- l, a `mod` 2 == 0]

{- Dado um booleano, decide por multiplicar todos elementos
   por 2 ou somar todos elementos com 5 -}
f2 :: Bool -> [Int] -> [Int]
f2 b l
  | b         = [a * 2 | a <- l]
  | otherwise = [a + 5 | a <- l]

------------------------------------------------------------
-- Agrupamento em duplas
------------------------------------------------------------

{- Fazer a função f0 que agrupa em duplas os cabeças
   de duas listas -}
f0 :: [t] -> [u] -> [(t, u)]
f0 _ [] = []
f0 [] _ = []
f0 (a:b) (c:d) = (a, c) : f0 b d

------------------------------------------------------------
-- Produto Cartesiano
------------------------------------------------------------

{- Objetivo da computação dado em sala de aula:
   Filtrar o resultado do produto cartesiano
   para gerar o resultado de f0
-}

{- Gera o produto cartesiano entre duas listas -}
f3 :: [t] -> [u] -> [(t, u)]
f3 l m = [(a, b) | a <- l, b <- m]

{- Desfaz a lista de duplas gerada por f3 -}
f3_aux01 :: [(t, u)] -> ([t], [u])
f3_aux01 l =
  ( [a | (a, b) <- l]
  , [b | (a, b) <- l]
  )

{- Conta as ocorrências repetidas de fst(dupla)
   da lista gerada por f3 -}
f3_count :: Eq t => [(t, u)] -> Int
f3_count [] = 0
f3_count [a] = 1
f3_count (a:b:x)
  | fst a == fst b = 1 + f3_count (b:x)
  | otherwise      = 1

------------------------------------------------------------
-- Separação por fst
------------------------------------------------------------

{- Gera lista contendo apenas as duplas
   cujo fst é igual ao parâmetro -}
f3_newList :: Eq u => u -> [(u, t)] -> [(u, t)]
f3_newList a l = [b | b <- l, a == fst b]

{- Remove da lista todas as duplas
   cujo fst é igual ao parâmetro -}
f3_newListDel :: Eq u => u -> [(u, t)] -> [(u, t)]
f3_newListDel a l = [b | b <- l, a /= fst b]

{- Gera lista de listas agrupando por fst -}
f3_newListofList :: Eq u => [(u, t)] -> [[(u, t)]]
f3_newListofList [] = []
f3_newListofList (a:x) =
  f3_newList (fst a) (a:x)
    : f3_newListofList
        (f3_newListDel (fst a) (a:x))

------------------------------------------------------------
-- Filtro do produto cartesiano
------------------------------------------------------------

{- Função que gera o filtro no produto cartesiano
   (possui observação no código original)
-}

f3_filtra :: Int -> [[(u, t)]] -> [(u, t)]
f3_filtra _ [] = []
f3_filtra i (a:b) =
  busca i a : f3_filtra (i + 1) b

{- Busca elemento da posição i -}
busca :: Int -> [(u, t)] -> (u, t)
busca i (a:b)
  | i /= 0    = busca (i - 1) b
  | otherwise = a

------------------------------------------------------------
-- Solução alternativa do Pedro
------------------------------------------------------------

{- Como usar a saída de f3 em uma função
   que retorne a computação de f0?
-}

f3P :: (Eq t, Eq u) => [t] -> [u] -> [(t, u)]
f3P l m =
  [(a, b) | a <- l, b <- m, f3_auxP l m a b]

f3_auxP :: (Eq t, Eq u) => [t] -> [u] -> t -> u -> Bool
f3_auxP (a:as) (b:bs) x y =
  (a == x && b == y)
    || f3_auxP as bs x y
f3_auxP _ _ _ _ = False

------------------------------------------------------------
-- Revisão
------------------------------------------------------------

------------------------------------------------------------
-- 01. Lista de duplas (Char, ASCII)
------------------------------------------------------------

listaDuplaCharInt :: [Int] -> [(Char, Int)]
listaDuplaCharInt [] = []
listaDuplaCharInt (x:xs) =
  (chr x, x) : listaDuplaCharInt xs

------------------------------------------------------------
-- 02. Pesquisa Char pelo ASCII
------------------------------------------------------------

meuChr :: Char -> [Char] -> Bool
meuChr _ [] = False
meuChr y (x:xs)
  | ord y == ord x = True
  | otherwise      = meuChr y xs

{- Versão usando List Comprehension -}
meuChr1 :: Char -> [Char] -> [Bool]
meuChr1 z y =
  [ord z == ord x | x <- y]

------------------------------------------------------------
-- 03. Pesquisa ASCII pelo Char
------------------------------------------------------------

meuOrd :: Int -> [Int] -> [Bool]
meuOrd z y =
  [chr z == chr x | x <- y]

------------------------------------------------------------
-- 04. Ordenação de lista de inteiros
------------------------------------------------------------

insertord :: Int -> [Int] -> [Int]
insertord a [] = [a]
insertord y (x:xs)
  | y <= x    = y : x : xs
  | otherwise = x : insertord y xs

ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (x:xs) =
  insertord x (ordenaLista xs)

------------------------------------------------------------
-- 05. Ordenação condicional
------------------------------------------------------------

{- Seja o tipo [(Bool, [Int])].

   Faça uma função que ordena [Int]
   quando o booleano é True.

   Também, passe o Bool para False
   quando ordenar [Int].

   Exemplo:

   ordenaListaDupla
     [(True,[3,4,1,0,9]),
      (False,[]),
      (True,[4,3,2,1,0])]

   Retorna:

     [(False,[0,1,3,4,9]),
      (False,[]),
      (False,[0,1,2,3,4])]
-}

ordenaListaDupla :: [(Bool, [Int])] -> [(Bool, [Int])]
ordenaListaDupla [] = []

ordenaListaDupla ((y, x):xs)
  | y =
      (False, ordenaLista x)
        : ordenaListaDupla xs

  | otherwise =
      (False, x)
        : ordenaListaDupla xs