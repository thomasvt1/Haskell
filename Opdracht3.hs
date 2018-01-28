module Opdracht3
	where
	import Data.List

	s = [1..6]
	stenen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
		
	count::Integer->[Integer]->Integer
	count c [] = 0
	count c (x:xs)
		|c==x= 1 + (count c xs)
		|otherwise = count c xs

	convert list = ([a,b,c,d,e,f],list) where
		a = count 1 list
		b = count 2 list
		c = count 3 list
		d = count 4 list
		e = count 5 list
		f = count 6 list
		
	n= length(stenen)
	amountOfStraigths=0

	
	--straight 0 = return()
	--straight n =
	--	do
	--		if((map fst (map convert stenen)!!n!!0) == 1 && (map fst (map convert stenen)!!n!!1) == 1 && (map fst (map convert stenen)!!n!!2) == 1 && (map fst (map convert stenen)!!n!!3) == 1 && (map fst (map convert stenen)!!n!!4) == 1) 
	--		then do amountOfStraigths++
	--		straight (n-1)
	
	countStraights n
		| n >= 0 = straight n + countStraights (n-1)
		| otherwise = 0
		
	straight n
		| map fst (map convert stenen)!!n!!0 == 1 && map fst (map convert stenen)!!n!!1 == 1 && map fst (map convert stenen)!!n!!2 == 1 && map fst (map convert stenen)!!n!!3 == 1 && map fst (map convert stenen)!!n!!4 == 1 = 1
		| map fst (map convert stenen)!!n!!1 == 1 && map fst (map convert stenen)!!n!!2 == 1 && map fst (map convert stenen)!!n!!3 == 1 && map fst (map convert stenen)!!n!!4 == 1 && map fst (map convert stenen)!!n!!5 == 1 = 1
		| otherwise = 0
