
module Shapes
( Point (..)
, Shape (..)
, Day (..)
, Tree (..)
, Blah(..)
, surface
, singleton
, treeInsert
, treeElem
) where

import Text.Read

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

surface:: Shape -> Float
surface (Circle _ r) = pi * r ^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 

class Blah a where
(.==):: a -> a -> Bool
(./=):: a -> a -> Bool
x .== y = not (x ./= y)
x ./= y = not (x .== y)

interactiveSumming = do
    putStrLn "Choose 2 nums"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy :: Maybe Double
    case mx of
        Just x -> case my of
            Just y -> putStrLn ("Sum is: " ++ show (x + y))
            Nothing -> retry
        Nothing -> retry
    where
    retry = do
        putStrLn "Invalid"
        interactiveSumming