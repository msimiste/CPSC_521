-- CPSC521-F2017
-- Mike Simister 10095107
-- Sept 26, 2017
-- Do questions 1,2,5,6,7,8,11,12,17,20,21. You should do the other questions in class, labs, and on your own!

--1. Write your own function for appending two lists and for reversing a list: 
--app:: ([a],[a]) -> [a], 
--rev:: [a] -> [a].  
app:: ([a],[a]) -> [a]
app ([], ys)= ys
app ((x:xs),ys) = x:(app(xs,ys))


rev:: [a] -> [a]
rev [] = []
rev [x] = [x]
rev xs = (last xs):(rev (init xs))

--myRev:: [a] -> [a]
--myRev xs = rev xs []
--    where
--        rev [] a = a
--        rev (x:xs) ys = rev xs (x:ys)


--Now define your own data type for lists and write the append function for this datatype.

-- Data Type was taken from class notes
data MyList a = Nil | Cons a (MyList a) deriving (Eq, Ord, Read, Show)

myListApp :: (MyList a, MyList a) -> MyList a
myListApp (Nil, xs) = xs
myListApp ((Cons x (xs)) ,ys) =  Cons x (myListApp (xs, ys))

--2. Write your own function for flattening a list of lists to a list: flatten :: [[a]] -> [a]. Write a flatten function for your own datatype for lists       
flatten:: [[a]] -> [a]
flatten []  = []
flatten xs = foldr (\acc x -> acc ++ x)[] xs

--myFlatten:: MyList (MyList a) -> MyList a
--myFlatten mls = foldr(\acc x -> acc ++ x) [] mls
              
-- ++3, Write a function which given a number and a list of numbers returns those numbers greater than the first number: greaterinlist:: Integer -> [Integer] -> [Integer].
greaterinlist:: Integer -> [Integer] -> [Integer]
greaterinlist _ [] = []
greaterinlist x ys = filter (>x) ys
--greaterinlist x ys = foldr (\b acc  -> if b > x  then b:acc else acc) [] ys

-- ++4. Write a function to determine whether its first argument, a list of integers, is lexicographically 
--larger than its second argument: lexInt::[Int] -> [Int] -> Bool.   
--Now modify it to work on any type in class Ord.
-- Finally can you modify the Ord class to include lexicographical orderings of lists.


--5. Write a function which splits a list into two lists, the first containing the odd indexed elements and the second containing the even indexed elements: msplit:: [a] -> ([a],[a]).
msplit:: [a] -> ([a],[a])
msplit [] = ([],[])
msplit [x] = ([x],[])
msplit (x:y:xs) = (x:(fst (msplit xs)),y:(snd (msplit xs)))

--helper for 5, no longer needed
keepEvens:: Integer -> [a] -> [a]
keepEvens _ [] = []
keepEvens c (x:xs) = if c `mod` 2 == 0 then x:(keepEvens (c+1) xs) else keepEvens (c+1) xs

--helper for 5, no longer needed
keepOdds:: Integer -> [a] -> [a]
keepOdds _ [] = []
keepOdds c (x:xs) = if c `mod` 2 == 1 then x:(keepOdds (c+1) xs) else keepOdds (c+1) xs

--6. Write a function to merge two lists of integers, taking the least first element at each step: mergeInt :: ([Integer],[Integer]) -> [Integer] 
mergeInt:: ([Integer],[Integer]) -> [Integer]
mergeInt ([],xs) = xs
mergeInt (xs,[]) = xs 
mergeInt((x:xs),(y:ys)) 
    | x <= y = x:mergeInt(xs,(y:ys))
    | otherwise = y:mergeInt((x:xs),ys)

--7. Write a mergesort for integers:  mergesortInt :: [Integer] -> [Integer].     
mergesortInt:: [Integer] -> [Integer]
mergesortInt [] = []
mergesortInt [x] = [x]
mergesortInt xs = mergeInt (mergesortInt a, mergesortInt b)
    where  
        (a,b) = splitAt n xs 
        n = (length xs) `div` 2 
    
--8. Generalize the mergesort to arbitrary ordered type (using the class system).
mergeGen:: (Ord a) => ([a],[a]) -> [a]
mergeGen(xs,[]) = xs
mergeGen ([],xs) = xs
mergeGen((x:xs),(y:ys)) 
    | x <= y = x:mergeGen(xs,(y:ys))
    | otherwise = y:mergeGen((x:xs),ys)
    
merge:: (Ord a) => [a] -> [a]
merge [] = []
merge [x] = [x]
merge xs = mergeGen (merge a, merge b)
    where  
        (a,b) = splitAt n xs 
        n = (length xs) `div` 2 

-- ++9. Write a quicksort for an arbitrary ordered type: quicksort:: (Ord a) => [a] -> [a].
-- ++10. Write a function which determines whether an element (of equality type) is in a list: member:: (Eq a) => a -> [a] -> Bool.

--11. Given a relation rel:: a -> b -> Bool and a list of a-elements and a list of b-elements write a 
--function which returns a list of pairs of an a-element and the list of b-elements from the second 
--list to which it is related: relgrp:: (a -> b -> Bool) -> [a] -> [b] -> [(a,[b])].  
--For example if the relation is the divide relation then 
--relgrp div [2,3] [1,2,3,4,5,6] = [(2,[2,4,6]),(3,[3,6])]].

relgrp:: (a -> b -> Bool) -> [a] -> [b] -> [(a,[b])]
relgrp f [] bs = []
relgrp f as [] = []
relgrp f (a:as) bs = case (a, filter (f a) bs) of 
    (_,[]) -> relgrp f as bs                             
    (x,_) -> d:(relgrp f as bs)
        where d =(a, filter (f a) bs)                                  

--In order to test relgrp function seen above
div':: Integer -> Integer -> Bool
div' 0 b = False
div' a 0 = True
div' a b
   | (b `mod` a) == 0 = True
   | otherwise = False
   
--12. Program the "group" function: given a predicate pred:: a -> a -> Bool and a list, 
--the group function breaks the list into a series of (maximal) sublists such that any two consecutive 
--elements satisfy the predicate, pred. 
--The type of the function group is group:: (a -> a -> Bool) -> [a] -> [[a]]. 
-- An example of its use is as follows: suppose that the predicate nbr determines whether the absolute 
--difference of two integers is at most 1 (i.e. they are equal or they differ by one) then group nbr 
--[2,1,3,4,5,5,4,7,4,3,3] = [[2,1],[3,4,5,5,4],[7],[4,3,3]] : program up this example to make sure your 
---group works.  What is group nbr []?

nbr::(Eq a, Num a) => a -> a -> Bool
nbr x y
    | (x - y) == 0 = True
    | abs (x - y) == 1 = True
    | otherwise = False

group:: (a -> a -> Bool) -> [a] -> [[a]]
group f xs = foldr(\x acc -> case acc of
    [] -> [x]:acc
    ((y:ys):zs) -> case (f x y) of
        True -> ((x:(y:ys)) : zs)
        False -> [x]:((y:ys):zs))[] xs  
    
generally::[(a,[b])] -> [(a,b)]
--generally list = foldr(\(a,xs) acc ->  (foldr(\t ac -> (a,t):ac)acc xs))[] list
generally list = foldr(\(a,xs) acc ->  (map (\t -> (a,t)) xs) ++ acc)[] list
--generally list = concatMap (\(a,xs) -> fmap(\x -> (a,x)) xs)  list  
--    ys -> if (f x (head (flatten ys))) then (([x] ++ (head (ys))) : (drop 1 acc)) else [x]:acc)[] xs


-- ++13. Write a function which given a list returns a list of all the subsets of the list: subset:: [a] -> [[a]].
-- ++14. Write a function which given a list returns the list of all permutations of that list: perm:: [a] -> [[a]].  As a bonus: given a permutation it is possible to give its cyclic decomposition. For example the permutation [2,4,5,1,3] of [1,2,3,4,5] can be represented as [[1,2,4],[3,5]] where this indicates that each element goes to it neighbor unless it is at the end of a sublist in which case it goes to the first in the sublist.
-- ++15. Write a function to turn decimal numbers into roman numerals and a function for the reverse translation (here is one solution, here is another).
-- ++16. Write programs to do basic matrix addition and multiplication.   For this excercise I want you  to regard a matrix as a list of lists of numbers and to define the operations in terms of the following primitive functions.  You will need a function to xzip two lists together which reports an error if they are not the same length (there is a zip in the prelude which does not report an error).  You will need the map function in the prelude.  You will need to write a function which transposes a matrix transpose:: [[a]] -> [[a]]  and to formad the dot product of two vectors.   You should be able to paste these basic functions together to define matrix multiplication.  

--17. Write a function for adding and multiplying polynomials.  
--You may represent the polynomials as lists of real numbers so [1,0,3,4.2] = 1 + 3x^2 +4.2x^3.   
--Thus addpoly:: [Float]  -> [Float] -> [Float] and multpoly:: [Float]  -> [Float] -> [Float].
addpoly:: [Float] -> [Float] -> [Float]
addpoly [] ys = ys
addpoly xs [] = xs
addpoly [x] (y:ys) = (x + y):(addpoly [] ys) 
addpoly (x:xs) (y:ys) = (x + y):(addpoly xs ys)

multpoly:: [Float] -> [Float] -> [Float]
multpoly [] _ = []
multpoly (x:xs) ys = final where
    first = multBy x ys
    rest = shiftByOne $ multpoly xs ys
    final = addpoly first rest

--helper function multiplies all elements of a list by n
multBy:: Float -> [Float] -> [Float]
multBy n xs = map (n*) xs

--helper function shifts the elements of a list to the right by 1 
shiftByOne:: [Float] -> [Float]
shiftByOne xs = 0:xs

-- ++18. Write a function which given a list of (real) roots of a polynomial produces a (real) polynomial with those roots.
-- ++19. Write a function which given a polynomial (as above) and a real number evaluates the poynomial at that number: evalpoly:: [Float]->Float -> Float.


--20. An expression tree (or term) is the datatype data ETree = Var String | Opn  String [ETree].  
--A typical element is Opn "add" [Var "x1",Var "x2"]  (which is an internal representation of x1 + x2 ).
--Write a function varsof:: Etree -> [String] which collects the set of variable in the expression (or term)
--Thus, varsof(Opn "add" [Var "x1",Var "x2"]) = ["x1","x2"].   
--Be careful to ensure that variables only occur once in the output list.
data ETree = Var String | Opn String [ETree]

varsof:: ETree -> [String]
varsof (Var s) = [s]
varsof (Opn s et ) = final where
    final = merge $ removeDupes $ foldr(\eTree acc -> (varsof eTree) ++ acc) [] et

--helper function for Q20    
removeDupes::(Eq a, Ord a) => [a] -> [a]
removeDupes [] = []
removeDupes [a] = [a]
removeDupes (x:xs) = case (x `elem` xs) of
        True -> removeDupes xs
        False -> x:(removeDupes xs)
 
 --21. Calculate the factorial of 1,891 or explain six different ways of programming factorial.
fact:: Integer -> Integer 
fact 0  = 1
fact n = n * fact(n-1)
--   factorial of 1891 =  10239307937318092035935715998267909916105924069923343459039300333411621494
--                        04719679358313030103629307499537675502273121155012574786008799074535267323
--                        48139697879240205809193824182578272238546947414141452728122331630359609722
--                        10319391171709719885131440264010925522750931824553177902460734805138986157
--                        94062593770138661420060228468986445133188814844334856496624129960355515846
--                        14794783176526943513848576295106324647430095738029177895198348132973882545
--                        27943010270592116587980672569418023668242209715447737881835639824653785259
--                        87742797110217162191502240942234194249374247600054990482663714323176087219
--                        23502826145594118934414962293515726602902207150065671425834202215137276193
--                        51672007920434464805608838586964157870698466489161336171506959227049518523
--                        39764766902339467723615819722522274457149696153974060325192815145653820898
--                        59393951865430523130947780180776248740345154932280886678623091653979464500
--                        87021486891382934430586271069345937594868962198665801856491990641475103459
--                        17253941542653977008615753153545363234680866475065688244584012937201035486
--                        07897960273073989189929471342161055766442127297198663340145325437713346507
--                        35974072534288615263888360314092805422208181750163978688318959310464745562
--                        68996823396305941656477538813583634483541076112659652235384676644954849238
--                        18883782528432182269477300177340734079763105606670198046243997482495855097
--                        64326000902616969879241529958140701552067611175771559385535570117086837053
--                        22726202715975853952147608125813702411854919504669102910089821728512870621
--                        74219095609436794433042908430228532576811611919180310565244857737732135159
--                        60653273862471541023458652072322539454509558311112969463398218570028055917
--                        76412388667273792439920505676559387179186292312321496269554009077586937961
--                        17596956893692071079688112306550822566556822785530715949608121562825051965
--                        76752970422858153277965189285477069146886260136194377695888523133818603207
--                        56778603607524144324301999012860477582638933394859625612956568275544981888
--                        25558332305082290480729573184047584052205604965824550386903885742814816278
--                        78795984103223977372601456347063441319564721587888878078992982747036715133
--                        95971825122259861536193043150387650175466220083448482679415741694097745242
--                        11319105204700406111818065940969523248784701986154270125612634692067767993
--                        35623830345192756475626315125115241902209025722571459253023633762172884317
--                        01942114988376365395514287778135537201341042093522605914347123033032161422
--                        50151136317079964776290800600070496198772594963583921129983689954537520744
--                        75178102429975439489845394247893324620903888728288998915587530159184332705
--                        15344127543151738888607967190096644067796511198745452180635781572357312235
--                        21306228012030442584136298374748254552837932067004349340556378800310187095
--                        53493086533283299598147902139029832397116444327302112570420579608733907005
--                        80247336729309097031374384830183960776848715963346286107186520908054966343
--                        64165187982110507158020356235903462398758316719242064655901235935581612107
--                        23844328317462947976507842730963842876400139505273899790354445000200060099
--                        11285319878436987883327828902050464514423216328308254975969640505078514733
--                        92850673213995906154586034451674088653219681791087140707468254281863894993
--                        73563281803841895119258421455600554770460497605320299846206852690587446856
--                        04612856806167366278224155547997162025192919945242744706858934935710835722
--                        46130072018797909530340071889131775004061754435263701406113784756613866230
--                        88301290304917407379720596721003283638208448183731877783436101188457376369
--                        57187977163890353331147682496553606410718433803509587824428962810475071400
--                        60480058812401717075096009824846347963694037879059780737346818818300445729
--                        93676379368051951815619406974151836866737539472241889309412747916502230356
--                        03910738571881226737792967770437134262531569278808665310311160640260803646
--                        29363401761840807708760283723307734658475952403076393754318240267845878744
--                        65731751042539320533355224955523335035022958596553115155837958131257636769
--                        01252028933608306157399017933741666194251053730498069492276186218579707601
--                        84215096148075622268425454269848613701382923487373233807070452679478610913
--                        66655410053313634873037885222456336094529393502806877589923103451760629465
--                        93973428492532324356829610917716306273390700779619981703885330264518191977
--                        84694773255427287351785882995617104291339161910931109554784522794672768030
--                        59638188989543226699861764336198101455254804633246409784970411473154347883
--                        84718575523013999085780176156040322624297159996666158936743574353443164128
--                        61704451205885089973066688221372959312989870297787509619952201254829312052
--                        74605925234725033979553820120816804492079163733760972552873509808462198633
--                        31464990690501217046013552060643098062910361833525733261578934376138102612
--                        07059546436517736387233077102375736461295736407269929172528308328030846140
--                        50477830391104978159573833804646305395237161874654616058825312221079232950
--                        55427558490983180171468236037187544637128195478637040247054354692612826372
--                        68408885232912510996673472646199642403955688400714744138892021058188800319
--                        73717065206427694399488000000000000000000000000000000000000000000000000000
--                        00000000000000000000000000000000000000000000000000000000000000000000000000
--                        00000000000000000000000000000000000000000000000000000000000000000000000000
--                        00000000000000000000000000000000000000000000000000000000000000000000000000
--                        00000000000000000000000000000000000000000000000000000000000000000000000000
--                        00000000000000000000000000000000000000000000000000000000000000000000000000
--                        00000000000000000000000000000000000000000000000000



