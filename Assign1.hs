
--1. Write your own function for appending two lists and for reversing a list: app:: ([a],[a]) -> [a], rev:: [a] -> [a].  Now define your own data type for lists and write the append function for this datatype.
myApp:: ([a],[a]) -> [a]
myApp ([], ys)= ys
myApp ((x:xs),ys) = x:(myApp(xs,ys))

myRev:: [a] -> [a]
myRev xs = rev xs []
    where
        rev [] a = a
        rev (x:xs) ys = rev xs (x:ys)
        
 --2. Write your own function for flattening a list of lists to a list: flatten :: [[a]] -> [a]. Write a flatten function for your own datatype for lists       
myFlatten:: [[a]] -> [a]
myFlatten []  = []
myFlatten xs = foldl(\acc x -> x ++ acc)[] xs
              
--3, Write a function which given a number and a list of numbers returns those numbers greater than the first number: greaterinlist:: Integer -> [Integer] -> [Integer].
greaterinlist:: Integer -> [Integer] -> [Integer]
greaterinlist _ [] = []
greaterinlist x ys = foldr (\b acc  -> if b > x  then b:acc else acc) [] ys

--4. Write a function to determine whether its first argument, a list of integers, is lexicographically larger than its second argument: lexInt::[Int] -> [Int] -> Bool.   Now modify it to work on any type in class Ord. Finally can you modify the Ord class to include lexicographical orderings of lists.


--5. Write a function which splits a list into two lists, the first containing the odd indexed elements and the second containing the even indexed elements: msplit:: [a] -> ([a],[a]).
msplit:: [a] -> ([a],[a])
msplit [] = ([],[])
msplit xs = (evens,odds)
    where
        evens = keepEvens 0 xs
        odds = keepOdds 0 xs

keepEvens:: Integer -> [a] -> [a]
keepEvens _ [] = []
keepEvens c (x:xs) = if c `mod` 2 == 0 then x:(keepEvens (c+1) xs) else keepEvens (c+1) xs

keepOdds:: Integer -> [a] -> [a]
keepOdds _ [] = []
keepOdds c (x:xs) = if c `mod` 2 == 1 then x:(keepOdds (c+1) xs) else keepOdds (c+1) xs

--6. Write a function to merge two lists of integers, taking the least first element at each step: mergeInt :: ([Integer],[Integer]) -> [Integer] 
mergeInt:: ([Integer],[Integer]) -> [Integer]
mergeInt ([],[]) = []
mergeInt ([],[x]) = [x]
mergeInt ([x],[]) = [x]
mergeInt ((x:xs),(y:ys)) = (min x y):(mergeInt (xs,ys))

 
--7. Write a mergesort for integers:  mergesortInt :: [Integer] -> [Integer].
--8. Generalize the mergesort to arbitrary ordered type (using the class system).
--9. Write a quicksort for an arbitrary ordered type: quicksort:: (Ord a) => [a] -> [a].
--10. Write a function which determines whether an element (of equality type) is in a list: member:: (Eq a) => a -> [a] -> Bool.
--11. Given a relation rel:: a -> b -> Bool and a list of a-elements and a list of b-elements write a function which returns a list of pairs of an a-element and the list of b-elements from the second list to which it is related: relgrp:: (a -> b -> Bool) -> [a] -> [b] -> [(a,[b])].  For example if the relation is the divide relation then relgrp div [2,3] [1,2,3,4,5,6] = [(2,[2,4,6]),(3,[3,6])]].
--12. Program the "group" function: given a predicate pred:: a -> a -> Bool and a list the group function breaks the list into a series of (maximal) sublists such that any two consecutive elements satisfy the predicate pred.   The type of the function group is group:: (a -> a -> Bool) -> [a] -> [[a]].  An example of its use is as follows: suppose that the predicate nbr determines whether the absolute difference of two integers is at most 1 (i.e. they are equal or they differ by one) then group nbr [2,1,3,4,5,5,4,7,4,3,3] = [[2,1],[3,4,5,5,4],[7],[4,3,3]] : program up this example to make sure your group works.  What is group nbr []?
--13. Write a function which given a list returns a list of all the subsets of the list: subset:: [a] -> [[a]].
--14. Write a function which given a list returns the list of all permutations of that list: perm:: [a] -> [[a]].  As a bonus: given a permutation it is possible to give its cyclic decomposition. For example the permutation [2,4,5,1,3] of [1,2,3,4,5] can be represented as [[1,2,4],[3,5]] where this indicates that each element goes to it neighbor unless it is at the end of a sublist in which case it goes to the first in the sublist.
--15. Write a function to turn decimal numbers into roman numerals and a function for the reverse translation (here is one solution, here is another).
--16. Write programs to do basic matrix addition and multiplication.   For this excercise I want you  to regard a matrix as a list of lists of numbers and to define the operations in terms of the following primitive functions.  You will need a function to xzip two lists together which reports an error if they are not the same length (there is a zip in the prelude which does not report an error).  You will need the map function in the prelude.  You will need to write a function which transposes a matrix transpose:: [[a]] -> [[a]]  and to form the dot product of two vectors.   You should be able to paste these basic functions together to define matrix multiplication.  
--17. Write a function for adding and multiplying polynomials.  You may represent the polynomials as lists of real numbers so [1,0,3,4.2] = 1 + 3x^2 +4.2x^3.   Thus addpoly:: [Float]  -> [Float] -> [Float] and multpoly:: [Float]  -> [Float] -> [Float].
--18. Write a function which given a list of (real) roots of a polynomial produces a (real) polynomial with those roots.
--19. Write a function which given a polynomial (as above) and a real number evaluates the poynomial at that number: evalpoly:: [Float]->Float -> Float.
--20. An expression tree (or term) is the datatype data ETree = Var String | Opn  String [ETree].  A typical element is Opn "add" [Var "x1",Var "x2"]  (which is an internal representation of x1 + x2 ).   Write a function varsof:: Etree -> [String] which collects the set of variable in the expression (or term).  Thus, varsof(Opn "add" [Var "x1",Var "x2"]) = ["x1","x2"].   Be careful to ensure that variables only occur once in the output list.
 --21. Calculate the factorial of 1,891 or explain six different ways of programming factorial.



