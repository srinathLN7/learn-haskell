{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import System.Environment
import Data.Monoid
import Data.Semigroup

type Value = Float
type Ident = String
type Table = [(Ident, Value)]
data Expr
  = Lit Value
  | Con Ident
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Seq [Expr]
  deriving Show

newtype Environment e a = Environment { runEnvironment :: e -> a }

instance Functor (Environment e) where
  fmap = liftM

instance Applicative (Environment e) where
  pure  = return
  (<*>) = ap

instance Monad (Environment e) where
  return :: a -> Environment e a
  return a = Environment $ \e -> a

  (>>=) :: Environment e a -> (a -> Environment e b) -> Environment e b
  ea >>= k = Environment $ \e ->
    let a = runEnvironment ea e in runEnvironment (k a) e

ask :: Environment e e
ask = Environment $ \e -> e

consult :: String -> String -> Environment Table Value
consult x s = do
  t <- ask
  case lookup x t of
    Just v  -> return v
    Nothing -> error s

evalReader :: Expr -> Environment Table Value
evalReader (Lit v)        = return v
evalReader (Con x)        = consult x $ concat [ "Looks like ", x, " is not available!" ]
evalReader (Add e f)      = do v <- evalReader e; w <- evalReader f; return (v + w)
evalReader (Sub e f)      = do v <- evalReader e; w <- evalReader f; return (v - w)
evalReader (Mul e f)      = do v <- evalReader e; w <- evalReader f; return (v * w)
evalReader (Div e f)      = do v <- evalReader e; w <- evalReader f; return (v / w)
evalReader (Seq [e])      = evalReader e
evalReader (Seq (e : es)) = do _ <- evalReader e; evalReader (Seq es)

