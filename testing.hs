module MyTests
(

) where

import System.Environment


data Bool' = True' | False'   deriving (Show)

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord, Read)
  
  
and':: Bool' -> Bool' -> Bool
and' True' True' = True
and' _ _ = False

add':: Nat-> Nat-> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)
