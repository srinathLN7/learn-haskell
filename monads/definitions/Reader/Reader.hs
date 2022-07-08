module Reader where

newtype Reader env a = Reader {runReader :: env -> a} 

-- make Reader an instance of Functor
instance Functor (Reader env) where
    -- fmap :: (a -> b) -> Reader env a -> Reader env b
    fmap g reader = Reader $ \ env -> g (runReader reader env)  


-- make Reader an instance of Applicative
instance Applicative  (Reader env) where 
    -- pure :: a -> Reader env a
    -- const :: a -> b -> a
     pure  = Reader . const  

     -- (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
     rf <*> ra = Reader $ \ env -> let g = runReader rf env 
                                       a = runReader ra env
                                       in g a   



-- make Reader an instance of the Monad
instance Monad (Reader env) where
    -- return :: a -> Reader env a
    return = pure

    -- (>>=) :: Reader env a -> (a -> Reader env b) -> Reader env b
    ra >>= g =  Reader $ \ env -> let a = runReader ra env
                                      in runReader (g a) env


ask :: Reader env env
ask = Reader $ \env -> env                                          