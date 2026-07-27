{- objetivo desta aula: Trabalhar os tipos lista e tuplas -}

--01- Operador que defina o menor entre dois inteiros
infix 7 &<&

(&<&) :: Int -> Int -> Int
x &<& y
    | x < y     = x
    | otherwise = y

--02- função que retorna o menor de uma [Int]
menorL::[Int]->Int
menorL [x] = x
menorL (x:xs) = x &<& menorL xs

--03- função que gera uma dupla com o menor e a lista de entrada
menorD::[Int]->(Int, [Int])
menorD xs = (menorL xs, xs)

--04- função que recebe uma dupla com o menor de uma lista e a lista e retorna a lista sem o menor

filtraMenor :: (Int, [Int])->[Int]
filtraMenor (_,[]) = []
filtraMenor (m,x:xs)
    | m == x    = xs
    | otherwise = x : filtraMenor (m,xs)

--05 função que recebe uma dupla com o menor e a lista restante e retorna uma lista ordenada
ordenaL :: (Int, [Int])->[Int]
ordenaL (m,[]) = [m]
ordenaL (m,xs) = m : ordena xs

--06 função interface para ordenar a lista de entrada
ordena::[Int]->[Int]
ordena [] = []
ordena [x] = [x]
ordena xs = ordenaL (menor, resto)
    where
        menor = menorL xs
        resto = filtraMenor (menor, xs)
