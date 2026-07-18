{- objetivo: trabalhar tipos distintos entre listas e tuplas -}

import Data.Char

------------------------------------------------------
{- 01 função que separa [(Int,Char)] em ([Int],[Char]) -}
myUnzipl :: [(Int,Char)]->([Int],[Char])
myUnzipl [] = ([],[])
myUnzipl ((x,y):xs) =
    (x : fst (myUnzipl xs), y : snd (myUnzipl xs))

{- 02 versão em uma única função -}
--myUnzipU :: [(Int,Char)]->([Int],[Char])

------------------------------------------------------------
{- 03 função que junta duas listas em lista de duplas -}
myZip::[Bool]->[Char] ->[(Bool,Char)]  
myZip [x] [y] = [(x,y)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys



{- 04 função que recebe [Char] e retorna [(Bool,Char)] 
   True se Char for alfanumérico e False, caso contrário -}

isAlphaNum2 :: Char -> Bool
isAlphaNum2 x
    | x >= 'a' && x <= 'z' = True
    | x >= 'A' && x <= 'Z' = True
    | x >= '0' && x <= '9' = True
    | otherwise = False

setAlfa:: String -> [(Bool,Char)] 
setAlfa [] = []
setAlfa (x:xs)
   | isAlphaNum2 x = (True, x) : setAlfa xs
   | otherwise = (False, x) : setAlfa xs
   
{- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos -}
filtraAlfa :: [(Bool,Char)] -> String
filtraAlfa [] = [] 
filtraAlfa ((y,ys):xs) = ys : filtraAlfa xs

{- 06 função transforma String de alfa em Int -}

alfaToInt::String -> [Int]
alfaToInt [] = []
alfaToInt (x:xs) = fromEnum x : alfaToInt xs


{-- 07 função que gera tabela ascii -}
geraASCII::Int->[(Int,Char)]
geraASCII x = [( x, chr x)]
