import System.Win32 (xBUTTON1)
{-Assunto: Litas
  Os conceitos introdutórios sobre listas foram apresentados em sala.
  Agora, considerando os casos mais simples, com apenas listas de inteiros,
  implemente as funções abaixo, considerando os operadores ++ e :
    ++ (concatena listas)
     : (insere um elemento na lista)
-}
     
{- 01 função que soma os elementos de uma lista -}
sumList::[Int]->Int
sumList [] = 0
sumList (x:xs) = x + sumList xs


{- 02-localiza elemento em lista -}
searchList::Int->[Int]->Bool
searchList a [] = False
searchList a (x:xs)
 | a == x = True
 | otherwise = searchList a xs


{-03 remove todas ocorrências de y em uma lista -}
deleteList::Int->[Int]->[Int]
deleteList y [] = []
deleteList y (x:xs)
 | y==x = deleteList y xs
 | otherwise = x : deleteList y xs
 
 
{-04 informa o tamanho de uma lista -} 
lengthList::[Int]->Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs


{-05 conta a ocorrência de um Int em [Int] -}
contList::Int->[Int]->Int
contList y [] = 0
contList y (x:xs)
 | y == x = 1 + contList y xs
 | otherwise = contList y xs


{- 06 inverte a lista -}
reverseList::[Int]->[Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

{- 07 inverte elementos das listas internas -}
reverseInside::[[Int]]->[[Int]]
reverseInside [] = []
reverseInside (x:xs) = reverseList x : reverseInside xs

{- 08 função que exclui a penúltima ocorrência de um número na lista-}
excluiPenultimo :: [Int] -> [Int]
excluiPenultimo [] = []
excluiPenultimo [a,b] = [b]
excluiPenultimo (x:y) = x : excluiPenultimo y
-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento
-}       
 
myHead :: [a] -> a
myHead [x] = x 
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [x] = [x]
myTail (x:xs) = xs

myLast :: [a] -> a
myLast [x] = x 
myLast (x:xs) = myLast xs  

myInit :: [a] -> [a]
myInit [y,z] = [y]
myInit (x:xs) = x : myInit xs

{- função que gera uma lista de booleanos relativa à comparação entre os elementos e um parâmetro z -} 
gBool :: [Int] -> Int -> [Bool]
gBool [] _ = []
gBool (a:x) z = (a>z):gBool x z

{- função que reverte uma lista -}
reverte :: [a] -> [a]
reverte   []  = []
reverte (b:x) = reverte x ++ [b]
     
fat::Int->Int
fat 0 = 1
fat x = x * fat(x-1)