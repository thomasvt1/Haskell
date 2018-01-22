module Vijf
  where

  import Data.Char
  
  keervijf::[Int]->[Int]
  keervijf[] = []
  keervijf (x:xs)=(5*x):keervijf xs
  
  
  keer::Int->[Int]->[Int]
  keer a[] = []
  keer a (x:xs)=(a*x):keer a xs
  
  apply::(a->b)->[a]->[b]
  apply f[] = []
  apply f (x:xs) = (f x):apply f xs