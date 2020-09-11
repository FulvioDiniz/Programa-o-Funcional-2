--Victor Guilherme Oliveira Santos - 11721BCC022
-- Fulvio Diniz Santos - 118BCC018
--1º Trabalho de Programação Funcional

--EX1
triangulo :: Float->Float->Float->String
triangulo a b c
    |a + b + c /= 180 = "Nao eh triangulo"
    |a == b && b == c = "Equilatero"
    |a > 90 || b > 90 || c > 90 = "Obtuso"
    |a == 90 || b == 90 || c ==90 = "Retangulo"
    |otherwise = "Simples"
 
 --EX2
equacao :: Double->Double->Double->(Double,Double)
equacao a b c 
    |a /= 0 = (x,y)
    |otherwise = (p,a)
 where 
  x = (-b+ sqrt d)/(2*a)
  y = (-b- sqrt d)/(2*a)
  d = (b^2)-(4*a*c)
  p = -c/b

  --Ex 3
bissexto:: Int -> Bool
bissexto x
    |(mod x 4) == 0 && (mod x 100) /= 0 ||(mod x 400) == 0 = True
    |otherwise = False
type Data = (Int,Int,Int)

bissexto2:: Data -> Bool
bissexto2 (d,m,a)
    |d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m ==8 || m == 10 || m == 12) && (bissexto a == True) = True
    |d>=1 && d<=30 && ( m == 4 || m == 6 || m == 9 || m == 11) && (bissexto a == True) = True
    |d>=1 && d<= 29 && (bissexto a == True) = True
    |otherwise = False

valida:: Data -> Bool
valida (d,m,a)
    |d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m ==8 || m == 10 || m == 12) && (bissexto a == True) = True
    |d>=1 && d<=30 && ( m == 4 || m == 6 || m == 9 || m == 11) && (bissexto a == True) = True
    |d>=1 && d<= 29 && (bissexto a == True) = True
    |d>=1 && d<=28 && (bissexto a == False) = True
    |otherwise = False

idade:: Data -> Data -> Int
idade (d,m,a) (d1,m1,a1)
    |valida (d,m,a) == False || valida (d1,m1,a1) == False = -1
    |m < m1 = a - a1 - 1
    |m == m1 && d < d1 = a - a1 - 1
    |m == m1 && d == d1 = a - a1
    |otherwise = a - a1

passagem:: Float -> Data -> Data -> Float
passagem v (d,m,a) (d1,m1,a1)
    |idade (d,m,a) (d1,m1,a1) < 2 = v * 0.15
    |idade (d,m,a) (d1,m1,a1) <= 10 = v * 0.40
    |idade (d,m,a) (d1,m1,a1) >= 70 = v * 0.50
    |otherwise = v

--Ex4
gera1:: [Int]
gera1 = [x^2|x<-[1..15],x>=4,x<=14,mod x 2 /= 0]

gera2:: [(Int,Int)]
gera2 = [(x,y) | x<-[1..15], x>=1, x<=4, y<- [x..2*x]]

gera3:: [Int] 
gera3 = [l1 | x<-[10..15], x >= 10, l1 <- [1..x]]

gera4:: [(Int,Int)]
gera4 = [(x,x+1) | x<-[1..16] ,odd x]

gera5:: [(Int)]
gera5 = [(x+y) |(x,y)<-gera4]

--Ex5
contneg :: [Int]->Int
contneg cont = length[x | x<-cont, x<0, mod x 2 == 0]

listneg:: [Int] -> [Int]
listneg y = [ x | x<-y, x<0, mod x 2 == 0]

--Ex6
distancia :: [(Float,Float)]->[Float]
distancia dist = [ sqrt (x^2 + y^2) | (x,y)<-dist]
    
--Ex7
fatores :: Int->[Int]
fatores n =[x | x<-[1..n], x<=n, mod n x == 0]

primos :: Int->Int->[Int]
primos x y = [ z | z<-[x..y], fatores z == [1,z]]

--Ex8
mdc :: Int -> Int -> Int
mdc m 0 = m
mdc m n = mdc n (mod m n)

mmc2 :: Int -> Int -> Int
mmc2 a b = div (a * b) (mdc a b)

mmc :: Int -> Int -> Int -> Int
mmc a b c = mmc2 a (mmc2 b c)

--Ex10
fizzbuzz2 :: Int -> String
fizzbuzz2 x
    | (mod x 3 == 0) && (mod x 5 == 0) = "FizzBuzz"
    | (mod x 3 == 0) = "Fizz"
    | (mod x 5 == 0) = "Buzz"
    | otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz x = [fizzbuzz2 x | x <- [1..x]]
--Ex12
procurando :: Int->[Int]->Bool
procurando x [] = True
procurando x (x1:x2) = if x == x1 then False else(procurando x x2)

unica_ocorrencia :: Int->[Int]->Bool
unica_ocorrencia x [] = False
unica_ocorrencia x (x1:x2) = if x == x1 then procurando x x2 else unica_ocorrencia x x2

--Ex13
intercala :: [Int] -> [Int] -> [Int]
intercala [] ls = ls
intercala ls [] = ls
intercala (l:ls) d = [l] ++ intercala d ls

--Ex14
type Contato = (String, String, String, String)

nome :: String -> [Contato] -> String
nome _ [] = "Email desconhecido"
nome x ((c, _, _, email):y) = if (x == email) then c else (nome x y)

--Ex15 
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas =  [ ("Rosa", 1.66, 27,'F'),("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S') ]
tamanho :: [Pessoa] -> Float
tamanho [] = 0
tamanho (x:xs) = 1 + (tamanho xs)

altTotal :: [Pessoa] -> Float
altTotal [] = 0
altTotal ((_, a, _, _):xs) = (a + altTotal xs)

altMedia :: [Pessoa] -> Float
altMedia x = (altTotal x) / tamanho x
    
maisNova :: [Pessoa] -> Int
maisNova x = minimum [i | (_, _, i, _) <- x]

maisVelho :: [Pessoa] -> String
maisVelho [(n, _, _, e)] = n ++ "(" ++ [e] ++ ")"
maisVelho ((n1, a1, i1, e1):(n2, a2, i2, e2):xs)
    |i1 > i2 = maisVelho ((n1, a1, i1, e1): xs)
    |otherwise = maisVelho ((n2, a2, i2, e2):xs)


maisCinq :: [Pessoa] -> [Pessoa]
maisCinq [] = []
maisCinq x = [(n2, a2, i2, e2) | (n2, a2, i2, e2) <- x, i2 >= 50]
    
casadas :: Int -> [Pessoa] -> [Pessoa]
casadas _ [] = []
casadas i x = [(n2, a2, i2, e2) | (n2, a2, i2, e2) <- x, i2 >= i, e2 == 'C']

--16
insere_ord :: Ord a => a -> [a] -> [a]
insere_ord _ [] = []
insere_ord elmnt (x:xs) = if (elmnt > x) then [x] ++ (insere_ord elmnt xs) else [elmnt] ++ (x:xs)

--17
reverte :: [a] -> [a]
reverte [] = []
reverte [a] = [a]
reverte (x:xs) = (reverte xs) ++ [x]

--18
membro :: Eq a => a -> [a] -> Bool
membro _ [] = False
membro a (x:xs) = if(a == x) then True else membro a xs

sem_repetidos :: Eq a => [a] -> [a]
sem_repetidos [] = []
sem_repetidos (x:xs) = if (membro x xs) then (sem_repetidos xs) else [x] ++ (sem_repetidos xs)

--19
disponiveis :: [Int]
disponiveis = [1,2,5,10,20,50,100]
notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco x = [ y:ys | y <- disponiveis, x >= y, ys <- notasTroco(x-y)]