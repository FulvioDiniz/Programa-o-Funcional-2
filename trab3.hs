Operador1 :: Bool -> Bool -> Bool
Operador1 _ _ = False
Operador1 _ True = True
Operador1 True _ = True


Operador2 :: Bool -> Bool -> Bool
Operador2 False False = False
Operador2 True True = True
Operador2 True False = True
Operador2 False True = True


Operador3 :: Bool -> Bool -> Bool
Operador3 True _ = True
Operador3 False b = b

Operador4 :: Bool -> Bool -> Bool
Operador4 bol1 bol2
    | bol1== True = True
    | bol2== True = True
    | otherwise = False


Operador5 :: Bool -> Bool -> Bool
Operador5 bol1 bol2
    | bol1== False && bol2 == False = False
    | otherwise = True


fatorial ::Int->Int
fatorial 0 = 1
fatorial n = n* (fatorial(n-1))

fibo :: Int->Int
fubo 0 = 0
fibo 1 = 1 
fibo n = fibo(n-1)+fibo(n-2)

n_tri :: Int -> Int
n_tri 0 = 0
n_tri 1 = 1
n_tri n = n + n_tri(n-1)

type Par = (Int, Int)

passo :: Par->Par
passo (x,y) = (y,x+y)

fibopar :: Int->Par
fibopar 0 = (0,1)
fibopar n = passo(fibopar(n-1))

fibo2 :: Int -> Int
fibo2 n = fst(fibopar(n))


prmeirofibo = [fibo2 x | x <- [0..50]]



potencia2 :: Int-> Int
potencia2 0 = 1
potencia2 n = 2* (potencia2 (n-1))


prodIntervalo ::Int->Int->Int
prodIntervalo 0 n = n
prodIntervalo m n = m * ((prodIntervalo (m-1)n))

resto_div :: Int->Int->Int
resto_div m n = if(m < n) then m else (resto_div(m-n) n)

div_inteira :: Int->Int->Int
div_inteira m n = if(m < n) then 0 else ((div_inteira(m-n) n) + 1)


mdc :: Int->Int->Int
mdc m n 
 |(n == 0) = m 
 | otherwise = mdc n (mod m n)
 
mdc2 :: Int->Int->Int
mdc2 m 0 = m
mdc2 m n = mdc n (mod m n)


binomial :: Int->Int->Int
binomial n k
 | (k ==0) || (k ==n)=0
 | otherwise = (binomial(n-1) k) + (binomial(n-1) (k-1))
 
binomial2 :: Int->Int->Int
binomial2 n 0 = 0
binomial2 n k = if(k == n)then 0 else (binomial(n-1) k) + (binomial(n-1) (k-1))



main2 = do
    print([5,4..1])
    print(['a','c'..'e'])
    print([1,4..16])
    print(zip [1, -2..(-11)] [1,5..17])

lista :: Int -> Int -> [Int]
lista a b
    | a == b = [a]
    | a > b = []
    | otherwise = a:(lista (a+1) b)

listaPares :: Int -> Int -> [Int]
listaPares a b
    | a == b = []
    | a + 1 == b = []
    | a > b = []
    | mod a 2 == 0 = listaPares (a+1) b
    | otherwise = (a+1):(listaPares (a+1) b)