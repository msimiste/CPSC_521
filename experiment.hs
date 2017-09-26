data  SF a = SS a | FF
data SM s a = SM (s -> (a,s))

runSM:: (SM s a) -> (s -> (a,s))
runSM (SM f) = f

instance Monad SF where
    return a = SS a
    FF >>= _ = FF
    SS a >>= f = f a

instance Monad (SM s) where
    return:: a -> (SM s a)
    return a = (\s -> (a,s))
    
    (>>=)::(SM s a) -> (a -> SM s b) -> (SM s b)
    SM f >>= g = SM(\s -> (\(a,s') -> runSM(g a) s')(f s))