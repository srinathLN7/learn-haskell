{-# LANGUAGE InstanceSigs, TypeApplications #-}

import System.IO
import Control.Monad
import Control.Monad.State

type Value = Int
type Ident = String
type Table = [(Ident, Value)]
data Expr
  = Lit Value
  | Var Ident
  | Set Ident Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

type Lives = Int
type Secret = Int

-- Monad Transformer
-- Monad Transformer is a type constructor which takes a monad and transforms it into a different monad

-- MaybeT
-- newtype MaybeT m a   = MaybeT { runMaybeT :: m (Maybe a) }

-- StateT
-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- ReaderT r (StateT s IO) a
-- Monad Stack
-- Reader
-- State
-- IO
--
-- transformers
-- mtl
--  - MonadIO
--  - MonadState
--  - MonadReader

-- instance Monad m => Functor (StateT s m) where
--   fmap = liftM
-- 
-- instance Monad m => Applicative (StateT s m) where
--   pure  = return
--   (<*>) = ap
-- 
-- instance Monad m => Monad (StateT s m) where
--   return :: a -> StateT s m a
--   return a = StateT $ \s -> return (a, s)
-- 
--   (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
--   sma >>= k = StateT $ \s0 -> do
--     (a, s1) <- runStateT sma s0
--     (b, s2) <- runStateT (k a) s1
--     return (b, s2)
-- 
-- lift :: IO a -> StateT s IO a 
-- lift ioa = StateT $ \s -> do
--   a <- ioa
--   return (a, s)
-- 
-- type World a = StateT Lives IO a
--
--
-- what Value is to Type, Type is to Kind
-- 4 :: Int, Int    :: *
--         , Maybe  :: * -> *
--         , Either :: * -> * -> *

play :: Secret -> StateT Lives IO a
play secret = do
  lives <- get
  case lives < 1 of
    True  -> do
      lift $ putStrLn "Game Over :("
    False -> do
      lift $ putStrLn $ show lives ++ " lives left."
      lift $ putStrLn "Please enter the guess."
      guess <- lift $ read <$> getLine
      case compare guess secret of
        EQ -> do
          lift $ putStrLn "Yay, you guessed correct :)\n"
        GT -> do
          lift $ putStrLn "Ah, you guessed too high!\n"
          StateT $ \s -> return ((), lives - 1)
          play secret
        LT -> do
          lift $ putStrLn "Ah, you guessed too low.\n"
          StateT $ \s -> return ((), lives - 1)
          play secret

getSecret :: IO Int
getSecret = do
  putStrLn "Please enter the secret."
  hSetEcho stdin False
  secret <- read <$> getLine
  hSetEcho stdin True
  putStrLn ""
  return secret

main :: IO ()
main = do
  secret <- getSecret
  runStateT (play secret) 7
  return ()
