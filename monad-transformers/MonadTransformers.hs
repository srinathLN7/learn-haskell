{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module MonadTransformers where
import Text.Read (readMaybe)


-- newtype MaybeIO a = MaybeIO (IO (Maybe a))
-- newtype MaybeIO a = MaybeIO {runMaybeIO :: IO (Maybe a)} 

-- MaybeIO captures the effect of Maybe inside IO monad
-- Main effect is Maybe and sub-effect is IO

-- Hence we can abstract out the effect of the IO monad


newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Functor (MaybeT m) where
  -- fmap :: (a -> b) -> MaybeT m a -> MaybeT m b 
  fmap g (MaybeT tma) = MaybeT $ do ma <- tma
                                    case ma of 
                                     Nothing -> return Nothing
                                     Just a ->  return $ Just (g a) 


instance Monad m => Applicative (MaybeT m) where
  -- pure :: a -> f a 
    pure = return 

  -- <*> :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  -- tmf :: m (Maybe (a -> b)) ; tma :: m (Maybe a)
    MaybeT tmf <*> MaybeT tma = MaybeT $ do  mf <- tmf
                                             case mf of
                                                    Nothing -> return Nothing
                                                    Just f -> do ma <- tma
                                                                 case ma of
                                                                  Nothing -> return Nothing
                                                                  Just a -> return $ Just (f a)  




instance Monad m => Monad (MaybeT m) where
    -- return :: a -> MaybeT m a 
    return =  MaybeT . return . Just

    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    MaybeT tma >>= f = MaybeT $ do ma <- tma
                                   case ma of
                                    Nothing -> return Nothing
                                    Just a -> runMaybeT (f a)




class (forall m. Monad  m => Monad (t m)) => MonadTrans t where
    lift :: Monad m => m a -> t m a 


instance MonadTrans MaybeT where
    -- lift :: Monad m => m a -> MayT m a 
    lift =  MaybeT . fmap Just 


-- readMaybe :: Read a => String -> Maybe a 
-- getLine :: IO String
readLine :: Read a => MaybeT IO a
readLine = do  
              str <- lift getLine
              MaybeT $ return $ readMaybe str 


computation :: MaybeT IO ()
computation = do a <- readLine 
                 b <- readLine
                 lift $ print $ show (a + b)   


main :: IO ()
main = const () <$> runMaybeT computation
-- main = putStrLn "Enter a number" >> readLn >>= (\ p -> putStrLn $ show (p+0))
