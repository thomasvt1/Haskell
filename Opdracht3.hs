module Opdracht3
    where
    import Data.List
    
    -- Deze functie differentieert de functie f numeriek in punt x met een precisie p
    -- y = a x + b
    
    -- Deze functie werk nog niet :(
    
    differentieer::(Double->Double)->Double->Double->Double
    differentieer f p x = x
    
    integreer::(Double->Double)->Double->Double->Double->Double
    integreer f a b p = p
    
    -- Deze functie heeft een lijst als invoer en levert als uitvoer een lijst met uitsluitend
    -- die elementen die meer dan ´e´en keer voorkomen in de lijst
    dubbelen::(Ord a)=>[a]->[a]
    dubbelen = map head . group . sort

