{-Objetivos: introduzir os conceitos de
   a) List Comprehension
   b) Função de alta ordem.
   
   Para tanto, iniciamos com um problema simples e mostramos,
   a cada passo, as possibilidades de melhorias.
   -}
import Data.Char

{-motivação-}
dobra_a::Int->Int
dobra_a x = 2 * x

mul_b::Int->Int->Int
mul_b a b = a * b

op_c::(Int->Int->Int)-> Int-> Int -> Int
op_c f x y = (f) x y

{-pode passar a função (* 9), por exemplo -}
op_d ::(Int->Int)-> Int-> Int
op_d f y = f y

{-escreva função de alta ordem para filtrar digito ou alpha -
  neste caso, o f pode ser isDigit ou isAlpha-}

filtraAO::(Char->Bool)->String->String
filtraAO f [] = []
filtraAO f (a:b)
  |f a       = a:filtraAO f b
  |otherwise =   filtraAO f b

{- reescrevendo com uso de list comprehension -}
farfar :: (Char->Bool)->String->String
farfar f l = [ x | x<-l, f x] 

{-As funções f1_p1 e f2_p2, a seguir, são específicas. A partir
de uma [String], elas geram [(Bool, String)] separando as strings
que só possuem dígitos das demais --}

-- questão 01
f1_p1::String->Bool
f1_p1 [] = True
f1_p1 (x:xs)
  |isDigit x = f1_p1 xs  
  |otherwise = False

-- questão 02
f2_p1::[String]->[(Bool, String)]
f2_p1 [] = []
f2_p1 (x:xs) = (f1_p1 x, x) : f2_p1 xs

{-generalizando a função f2_p1 -}

f2_p1_generica :: (b -> a) -> [b] -> [(a, b)]
f2_p1_generica  f l = [(f a,a)|a<-l]

{- Mas, essas funções podem ser generalizadas.
 Pensemos em uma solução recebe uma [String] e retorna uma [(Bool, String)]
 em que o Bool é True se o String atende a uma característica determinada.
 Para tanto, devemos implementar as características desejaveis -}

cr01_Todos_Char::(Char->Bool)->String->Bool
cr01_Todos_Char f [] = True
cr01_Todos_Char f (x:xs) = f x && cr01_Todos_Char f xs 
  

cr01_Algum_Char::(Char->Bool)->String->Bool
cr01_Algum_Char _ [] = False
cr01_Algum_Char f (x:xs) = f x || cr01_Todos_Char f xs 

{-observe que todos ou alguns podem ser generalizados, também. 
  Para isso, basta passar o operador lógico && ou || -}

cr01_A_T_Char::(Bool->Bool->Bool)->(Char->Bool)->String->Bool  
cr01_A_T_Char _  _ [] = False
cr01_A_T_Char f g (x:xs) = f (g x) (cr01_A_T_Char f g xs) 

{- as funções abaixo filtram de [(Bool, String)] a [String]
   de acordo com o Booleano -}
    
{-como transformá-las em uma função genérica? -}

filtraT::[(Bool, String)]->[String]
filtraT [] = []
filtraT (x:xs)
  |fst x = snd x : filtraT xs
  |otherwise = filtraT xs

filtraF::[(Bool, String)]->[String]  
filtraF (x:xs)
  |not (fst x) = snd x : filtraF xs
  |otherwise = filtraF xs

{--------------------solução---------------------}
filtraTF::(Bool->Bool)->[(Bool, String)]->[String]
filtraTF _ [] = [] 
filtraTF n (x:xs)
  |n (fst x) = snd x : filtraTF n xs
  |otherwise = filtraTF n xs

seletor :: Bool -> Bool
seletor x = x
inversor :: Bool -> Bool
inversor x = not x

{------------------------------------------------}

------------------  revisão e uso de alta ordem ----------------------------
{- Considere f1 capaz de somar uma lista de inteiros se um Char for alfanumérico, 
    ou multiplicar os elementos, caso contrário -}
f1::Char->[Int]->Int
f1 c x
  |isDigit c && x == [] = 0
  |not(isDigit c) && x == [] = 1
  |isDigit c = y + f1 c xs
  |otherwise = y * f1 c xs
    where (y:xs) = x
    
{- reescreva f1 usando casamento de padrão -}
f2 :: Char -> [Int] -> Int
f2 c []
  | isDigit c = 0
  | otherwise = 1

f2 c (x:xs)
  | isDigit c = x + f2 c xs
  | otherwise = x * f2 c xs

{- reescreva f2 fazendo chamadas de funções para somar ou multiplicar -}

f3 :: Char -> [Int] -> Int
f3 c []
  | isDigit c = 0
  | otherwise = 1

f3 c (x:xs)
  | isDigit c = soma x  (f2 c xs)
  | otherwise = multiplica x (f2 c xs)


soma:: Int -> Int -> Int
soma x y = x + y

multiplica:: Int -> Int -> Int
multiplica x y = x * y


{- reescreva f3 usando função de alta ordem para definir o operador
   Esta função é didática, pois mostra o uso de função de alta ordem
   Neste caso, considere que a lista tem, pelo menos, um elemento -}
 {- para lista de pelo menos um elemento -}

f4 :: (Int -> Int -> Int) -> [Int] -> Int
f4 op [x] = x
f4 op (x:xs) = op x (f4 op xs)


{- faça a função myMap aplica uma função a cada elemento de uma lista -}

myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs 


{- função que converte caixa baixa para caixa  alta
   usar a função myMap para aplicar a uma String -}
maiuscula :: Char -> Char
maiuscula x 
  |x >= 'a' && x <'z' = chr(ord x-32)
  |otherwise = x

caixaalta :: String -> String
caixaalta [] = []
caixaalta x = myMap maiuscula x

{-permite escolher subtrair elemento da lista pelo parâmetro junto ao operador 
  Ex: map (meusub 3) [3,6,5,7] -}  
meusub:: Int -> Int -> Int
meusub x y = y - x  

{- pode ser aplicado ao map, mas é pouco genérico -}

meuOp :: (Int -> Int -> Int) -> Int -> Int -> Int
meuOp op x y = op y x


{-solução que testa se todos elementos de uma lista são dígitos -}
meuteste x y = x && isDigit y

f1_prova ::String -> Bool
f1_prova s = foldl meuteste True s 