--exercícios de list comprehension

{- 01 multiplica todos os elementos da lista por 3
   ex1 [1,2,3,4] retorna [3,6,9,12] -}
ex1 :: [Int] -> [Int]
ex1 l = [x*3|x<-l]

{- 02 retorna apenas os números pares da lista
   ex2 [1,2,3,4,5,6] retorna [2,4,6] -}
ex2 :: [Int] -> [Int]
ex2 l = [x|x<-l, mod x 2 == 0]

{- 03 retorna o quadrado dos números ímpares
   ex3 [1,2,3,4,5] retorna [1,9,25] -}
ex3 :: [Int] -> [Int]
ex3 l = [x*x|x<-l, mod x 2 /= 0]

{- 04 converte uma String para uma lista de ASCII (dica: ord)
   ex4 "ABC" retorna [65,66,67] -}
--ex4 :: String -> [Int]
--ex4 l = [ x | x<- l]

{- 05 converte uma lista ASCII para String (dica: chr)
   ex5 [65,66,67] retorna "ABC" -}
--ex5 :: [Int] -> String
--ex5 l = [chr x|x<-l]

{- 06 recebe uma lista de duplas e retorna apenas os primeiros elementos
   ex6 [(1,'a'),(2,'b'),(3,'c')] retorna [1,2,3] -}
ex6 :: [(Int,Char)] -> [Int]
ex6 l = [x|(x,_)<-l]

{- 07 recebe uma lista de duplas e retorna apenas as duplas cujo primeiro elemento é maior que 10
   ex7 [(5,'a'),(20,'b'),(7,'c'),(15,'d')] retorna [(20,'b'),(15,'d')] -}
ex7 :: [(Int,Char)] -> [(Int,Char)]
ex7 l = [(x,y) |(x,y)<-l, x>10]

{- 08 gera o produto cartesiano entre duas listas
   ex8 [1,2] ['a','b'] retorna [(1,'a'),(1,'b'),(2,'a'),(2,'b')] -}
ex8 :: [a] -> [b] -> [(a,b)]
ex8 l m = [(x,y)| x <- l , y <- m]

{- 09 recebe uma lista de inteiros e retorna uma lista de duplas (numero, quadrado)
   ex9 [1,2,3] retorna [(1,1),(2,4),(3,9)] -}
ex9 :: [Int] -> [(Int,Int)]
ex9 l = [(x,x*x)| x<- l]

{- 10 recebe uma lista de duplas e retorna apenas as duplas cujo fst aparece mais de uma vez
   ex10 [(1,'a'),(2,'b'),(1,'c'),(3,'d'),(2,'e')] retorna [(1,'a'),(2,'b'),(1,'c'),(2,'e')] -}
ex10 :: Eq a => [(a,b)] -> [(a,b)]
ex10 l = [ p| p <- l , length [ q | q <- l, fst q == fst p ] > 1 ]

{- 11 recebe uma String e retorna apenas as vogais
   ex11 "banana" retorna "aaa" -}
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

ex11 :: String -> String
ex11 l = [x|x<-l,isVowel x]

{- 12 recebe uma lista de inteiros e retorna apenas os números positivos
   ex12 [-3,4,-1,7,0,-5] retorna [4,7] -}
ex12 :: [Int] -> [Int]
ex12 l = [x|x<-l,x>0]

{- 13 recebe uma lista de inteiros e retorna uma lista de duplas (numero, paridade)
   ex13 [1,2,3,4] retorna [(1,False),(2,True),(3,False),(4,True)] -}
ex13 :: [Int] -> [(Int,Bool)]
ex13 l = [(x,mod x 2 == 0)|x<-l]

{- 14 recebe uma lista de duplas e retorna apenas os segundos elementos
   ex14 [(1,'a'),(2,'b'),(3,'c')] retorna "abc" -}
ex14 :: [(a,b)] -> [b]
ex14 l = [x|(_,x)<-l]

{- 15 (desafio) recebe uma lista de duplas e gera uma lista contendo todas
   as duplas que possuem o mesmo fst do primeiro elemento da lista
   ex15 [(1,'a'),(1,'b'),(2,'c'),(1,'d')] retorna [(1,'a'),(1,'b'),(1,'d')] -}
ex15 :: Eq a => [(a,b)] -> [(a,b)]
ex15 (l:ls) = [x|x<-(l:ls), fst x == fst l]