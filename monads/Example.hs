module Example where    

import Prelude hiding (fmap)    

data Expr a =   Var a 
              | Val Int 
              | Add (Expr a) (Expr a) 
               deriving Show 


expr :: Expr Char
expr = Add (Add (Val 1) (Var 'x') )  (Add (Val 2) (Var 'y'))

instance Functor Expr where 
    -- fmap :: (a -> b) -> Expr a -> Expr b
fmap g (Var x) = Var (g x)
fmap _ (Val n) = Val n 
fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
    -- pure :: a -> Expr a
  pure  = Var  

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b 
  Var g <*> Var a = Var (g a)
  Var g <*> Add x y = Add (Var g <*> x) (Var g <*> y) 
  _ <*> Val n = Val n
  Val n <*> _ = Val n  
  Add gx gy <*> x = Add (gx<*>x) (gy<*>x)


instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b 

  (Var x) >>= g = g x
  (Val n) >>= _ = Val n
  Add x y >>= g = Add (x >>= g) (y >>= g)        