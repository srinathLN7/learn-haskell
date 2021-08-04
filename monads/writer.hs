--  A monad example that captures the idea of computations while producing log outputs

import Text.Show (Show)

data Writer a = Writer a [String] deriving Show

number :: Int -> Writer Int 
number n = Writer n ["number: " ++ show n]

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int 
foo (Writer a as) (Writer b bs) (Writer c cs) = Writer (a+b+c) $ as ++ bs ++ cs 
-- During  run time call foo (number 7) (number 1) (number 3)

-- foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int 
-- foo' ($ number a) (number b) (number c) = number $ a+b+c 
-- This wont work as the functions are evaluated only during the run time and not during the compile time.


tell :: [String] -> Writer ()
tell  = Writer () -- tell str = Writer () str

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

foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = x `bindWriter` (\k ->
            y `bindWriter` (\l ->
            z `bindWriter` (\m ->
            let s = k + l + m
            in tell ["sum: " ++ show s] `bindWriter` (\_ ->
               Writer s [] ))))