import Data.Char

--PROVA 2025/1

-- Questão 1
pertenceLista :: Int -> [Int] -> Bool
pertenceLista _ [] = False
pertenceLista y (x:xs)
    |y == x = True 
    | otherwise = pertenceLista y xs

--Questão 2
pertencetupla :: Int -> [[Int]] -> [(Bool, [Int])]
pertencetupla _ [] = []
pertencetupla y (x:xs)
    | pertenceLista y x = (True,x) : pertencetupla y xs
    | otherwise = (False,x) :pertencetupla y xs

--Questão 3
pertencetripla:: Int -> [[Int]] -> (Int,[(Bool,[Int])])
pertencetripla y [] = (y, [])
pertencetripla y x  = ( y , pertencetupla y x )

--Questão 4
listaFalse::(Int,[(Bool,[Int])])->[[Int]]
listaFalse (x,[]) = []
listaFalse (x , ((y,ys):xs))
    |y == False = ys : listaFalse (x,xs)
    | otherwise = listaFalse (x,xs)

parametro1 :: Int -> [[Int]] -> [[Int]]
parametro1 x y = listaFalse (pertencetripla x y)

--Questão 5
compara:: [Char] -> [Char] -> [Bool]
compara [] _ = []
compara _ [] = []
compara (x:xs) (y:ys)
    |x == y = True : compara xs ys
    | otherwise = False : compara xs ys


    --Questão 1 prova
    
pertence :: String -> Bool
pertence [] = True
pertence (x:xs)
    | x >= '0' && x <= '9' = pertence xs
    | otherwise = False

--minha questao
pertenceg :: String -> Bool
pertenceg [] = True
pertenceg (x:xs)
    | x >= '0' && x <= '9' = False
    | otherwise = pertenceg xs