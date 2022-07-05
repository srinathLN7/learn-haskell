--  A monad example that captures the idea of computations while producing log outputs

import Text.Show (Show)
import GHC.Base (Monad)
import Control.Monad

data Writer a = Writer a [String] deriving Show

number :: Int -> Writer Int 
number n = Writer n ["number: " ++ show n]

-- Explicit accumulation of logs are required
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int 
foo (Writer a as) (Writer b bs) (Writer c cs) = Writer (a+b+c) $ as ++ bs ++ cs 
-- During  run time call foo (number 7) (number 1) (number 3)

-- foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int 
-- foo' ($ number a) (number b) (number c) = number $ a+b+c 
-- This wont work as the functions are evaluated only during the run time and not during the compile time.


tell :: [String] -> Writer ()
tell  = Writer () -- tell str = Writer () str


-- Explicit accumulation of logs are required
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' (Writer a as) (Writer b bs) (Writer c cs) =
     let 
         s = a + b + c 
         Writer _ us = tell ["sum: " ++ show s]
      in 
          Writer s $ as ++ bs ++ cs ++ us     

-- Define bindWriter
bindWriter :: Writer a -> (a -> Writer b) -> Writer b 
bindWriter (Writer x xs) f =
    let 
      Writer y ys  = f x
      in 
          Writer y $ xs ++ ys 

-- No explicit accumulation of logs are required
-- The logs are wrapped in the writer monad
foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = x `bindWriter` (\k ->
              y `bindWriter` (\l ->
              z `bindWriter` (\m ->
              let s = k + l + m
               in tell ["sum: " ++ show s] `bindWriter` (\_ ->
                Writer s [] ))))

-- No explicit accumulation of logs are required
-- The logs are wrapped in the writer monad
fooDo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
fooDo x y z =  do   k <- x
                    l <- y
                    m <- z
                    let s = k+l+m in 
                     Writer s ["sum: " ++ show s]        

-- Defind the Writer type as a custom monad
instance Functor Writer where
    fmap = liftM 
instance Applicative Writer where
    pure = return 
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter


-- Examples

x :: Writer Int
x = foo (number 1) (number 2) (number 3)

y :: Writer Int
y = foo' (number 1) (number 2) (number 3)

z :: Writer Int
z = foo'' (number 1) (number 2) (number 3)

z' :: Writer Int
z' = fooDo (number 1) (number 2) (number 3)
