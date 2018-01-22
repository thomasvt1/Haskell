module Opdracht2
    where
    import Data.Char (ord, chr)
    
    euclid::Int->Int->Int
    euclid x 0 = x
    euclid x y = euclid y (x`mod`y)
    
    egcd :: Int -> Int -> (Int,Int,Int)
    egcd 0 b = (b, 0, 1)
    egcd a b =
        let (g, s, t) = egcd (b `mod` a) a
        in (g, t - (b `div` a) * s, s)
    
    -- a = e, b = m'
    mijnegcd a b
        |x<0 =  let (g, s, t) = egcd (b `mod` a) a
                in (g, t - (b `div` a) * s + m', s)
        |otherwise = egcd a b
        where x = middelste(egcd a b)
    
    middelste (_,b,_) = b
    
    prime :: Int -> Bool
    prime n = (n > 1) && all (\ x -> rem n x /= 0) [2..n-1]
    
    --kies twee priemgetallen [x|x<-[100..500],prime x]
    p=347
    q=443
    
    --modulus m = 153721
    m=p*q
    
    --m' eulers = 152932
    m'=(p-1)*(q-1)
    
    --e<m' ggd=1 kies er 1 [x|x<-[1..m'],gcd x m' ==1]
    e=139493
    
    --d = 64853
    d= middelste(mijnegcd e m')
    
    eerste (a,_) = a
    tweede (_,b) = b
    rsaencrypt::(Int, Int)->Int->Int
    rsaencrypt (e, m) x = (x+eerste(e,m)^5)+tweede(e,m)
    
    rsadecrypt::(Int, Int)->Int->Int
    rsadecrypt (e, m) x = (x-tweede(e,m)-eerste(e,m)^5)

    textencrypt::(Int, Int)->Char->Int
    textencrypt (e, m) a = ord a+e+m
    
    textdecrypt::(Int, Int)->Int->Char
    textdecrypt (e, m) a = chr (a-e-m)
    
    
--Opdracht 5
--Asymetrische encryptie
-- Alice moet het de public key van Bob encrypten.
-- Bob moet het encrypten met de public key van Alice.
-- Beiden moeten met hun eigen public key decrypten.