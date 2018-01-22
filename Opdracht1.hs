module Opdracht1
  where
	
  som x y z = x+y+z

  mult x y= x*y

  faca::Int->Int
  faca 0 = 1
  faca x = x*faca(x-1)

  facb::Int->Int
  facb x
    |x<=0 = 1
    |otherwise = x*facb(x-1)

  nulpuntena::Double->Double->Double->[Double]
  nulpuntena a b c =
	if (b^2 - 4*a*c)>=0
		then [(-b + sqrt(b^2 - 4*a*c))/2*a,(-b - sqrt(b^2 - 4*a*c))/2*a]
	else []
	
  nulpuntenb::Double->Double->Double->[Double]
  nulpuntenb a b c
	|(b^2 - 4*a*c)<0 = []
	|otherwise = [(-b + sqrt(b^2 - 4*a*c))/2*a,(-b - sqrt(b^2 - 4*a*c))/2*a]
	
	
  rollDice2:: Int-> [(Int, Int, Int)]
  rollDice2 x = [(a,b,c)|a<-[1..6],b<-[1..6],c<-[1..6],som a b c `mod` x==0]