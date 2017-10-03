newtype State sa = State {runState:: s -> (s,a)}

instance Monad State where
    return:: a -> State sa
    return a = State (\s -> (s,a))
    
    
    
    (>>=):: State sa -> (a -> State sb) -> State sb
    
    State st >>= f  = State st' where
        st' = \s let (a,st') = runstate st ::(s,a)
                    State g = fa
                    in g s'

                    
-- Example using a symbol table

newtype ST = [(String,Int)]

push::String -> State ST()
push str = State (\s -> case s of
       [] -> [(str,0)],()
       (str',n):ts -> ((str,n+1):(str',n):ts,())

pop::State ST Int
pop = State (\s -> case s of 
    [] -> error "oops"
    (str,n):ts -> (ts,n)
    
get::String -> State ST Int
get str = State(\s -> (s, lookup str ) where
    lookup::String -> ST -> int
    lookup _ [] -> error "oops"
    lookup str ((str,n)):ts =
        | str == str' = n
        |otherwise = lookup str ts

test::String -> state ST Int
test str = do
        push str
        push str ++ str
        pop
        get str
 
runtest:: String -> ST -> Int
runtest str st = runstate