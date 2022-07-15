{-# LANGUAGE InstanceSigs #-}

import qualified Data.Map as Map
import Control.Monad.State hiding (get, put, modify)
import Data.Maybe (fromMaybe, fromJust)


newtype Transformer s a = Transformer { runTransformer :: s -> (a, s) } 

instance Functor (Transformer s) where
  fmap  = liftM

instance Applicative (Transformer s) where
  pure  = return
  (<*>) = ap

instance Monad (Transformer s) where
  return :: a -> Transformer s a
  return a = Transformer $ \s -> (a, s)

  (>>=) :: Transformer s a -> (a -> Transformer s b) -> Transformer s b
  sa >>= k = Transformer $ \s0 ->
    let (a, s1) = runTransformer sa s0
        (b, s2) = runTransformer (k a) s1 in
        (b, s2)

get :: Transformer s s 
get   = Transformer $ \s -> (s, s)

put :: s -> Transformer s ()
put s = Transformer $ \_ -> ((), s)

put' :: s -> a -> Transformer s a
put' s a = Transformer $ \_ -> (a, s)

-- EXAMPLE 1

type Value = Int
type Ident = String
type Table = [(Ident, Value)]  
data Expr
  = Lit Value
  | Var Ident
  | Set Ident Value
  | Add Expr Expr
  | Sub Expr Expr
  | Seq [Expr]
  deriving Show


eval :: Expr -> Table -> (Value, Table)
eval (Lit v)        = \t -> (v, t)
eval (Var x)        = \t ->
  case lookup x t of
    Just v  -> (v, t)
    Nothing -> error $
      concat [
          "Looks like "
        , x
        , " is not in scope!"
      ]
eval (Set x v)      = \t -> (v, (x, v) : t)
eval (Add e f)      = \t0 ->
  let (v, t1) = eval e t0
      (w, t2) = eval f t1 in
      (v + w, t2)
eval (Sub e f)      = \t0 ->
  let (v, t1) = eval e t0
      (w, t2) = eval f t1 in
      (v - w, t2)
eval (Seq []) = \ t0 -> (undefined, t0)
eval (Seq [e])      = \t0 ->
  let (v, t1) = eval e t0 in
      (v, t1)
eval (Seq (e : es)) = \t0 ->
  let (_, t1) = eval e t0
      (w, t2) = eval (Seq es) t1 in
      (w, t2) 


-- Implement eval using `do notation` with Transformer monad

eval' :: Expr -> Transformer Table Value
eval' (Lit v) = return v
eval' (Var x) =  do 
    table <- get 
    return (fromJust $ lookup x table)  
eval' (Set x v) = do
    table <- get
    put' ((x,v) : table) v 
eval' (Add e f) = do
     v <- eval' e
     w <- eval' f
     return (v+w)
eval' (Sub e f) = do
     v <- eval' e
     w <- eval' f
     return (v-w)
eval' (Seq []) = return undefined
eval' (Seq [e]) = do 
  result <- eval' e
  return result         
eval' (Seq (e:es)) = do
   eval' e
   vs <- eval' (Seq es)
   return vs 
   


-- Test example

e = Seq
  [
    Set "x" 2,
    Set "y" 7,
    Set "z" 17,
    Add (Var "x") (Var "z")
  ]



-- EXAMPLE 2

type Name     = String
type Amount   = Int
data Account  = Account { name :: Name, balance :: Amount }
  deriving Show
 
-- withdraw :: Amount -> Account -> ((), Account)
-- withdraw amount (Account name balance) = ((), Account name (balance - amount))

withdraw :: Amount -> Transformer Account ()
withdraw amount = do
  Account name balance <- get
  put (Account name (balance - amount))
  
-- deposit :: Amount -> Account -> ((), Account)
-- deposit amount (Account name balance) = ((), Account name (balance + amount))

deposit :: Amount -> Transformer Account ()
deposit amount = do
  Account name balance <- get
  put (Account name (balance + amount))
  
-- isDrawable :: Account -> (Bool, Account)
-- isDrawable account@(Account _ balance) = (balance > 0, account)

isDrawable :: Transformer Account Bool
isDrawable = do
  Account _ balance <- get
  return (balance > 0)

-- transaction :: Account -> Account
-- transaction a0 =
--   let a1 = withdraw 100 a0
--       a2 = if isDrawable a1 then withdraw 100 a1 else a1
--       a3 = if isDrawable a1 then withdraw 100 a1 else a1 in
--       a3

transaction :: Int -> Transformer Account Bool
transaction 0 = return True
transaction n = do
  withdraw 100
  t1 <- isDrawable
  case t1 of
    True  -> transaction (n - 1)
    False -> return False
