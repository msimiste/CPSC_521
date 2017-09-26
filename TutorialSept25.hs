data List a = Nil | Cons a (List a)

instance Functor LIst where
    fmap:: (a -> b) -> List a -> LIst b
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)
    
instance Applicative List where
    pure:: a -> List a
    pure a = Cons a Nil
    (<*>):: List(a -> b) -> List a -> List b
    Cons lf <*> Cons a la = flatten (helper lf a) (lf la)
    _ <*> _ = Nil
    
instance Monad List where
    return:: a -> List a
    return a = Cons a Nil
    (>>=):: List a -> (a -> List b) -> List b
    Nil >>= f = Nil
    Cons a la >>= f = Cons (f a) >>=  flatten Cons (f a) la >>= fl

instance Monoid list where
    mempty:: List a
    mappend::List a -> List a -> List a
    mempty = Nil
    mappend = (++)
    
flattn List (List a) -> List a
flatten lst = do 
    x <- lst
    y <- x
    return y
    
    <=>
    
flatten lst = lst >>= (\x -> >>= (\y -> return y))

flatten lst = case lst of
    Nil -> (\x -> >>= \y -> return y)
    
--cannoical example:
[x | x <- [1..10] , (x `mod` 2)==0]
[1..10] (>>=) =  (\x -> (guard (x `mod` 2) == 0)) >>
                return [x])
[1..10] (>>=) = (\x -> if (x `mod` 2 == 0) then return [x] else return
    
helper:: List(a->b) -> a -> List b
helper Cons f fl a = Cons (f a) (helper fl a)
helper Nil _ = Nil