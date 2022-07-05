module Example where
import Text.Read (readMaybe)

-- MaybeIO captures the effect of Maybe inside IO monad
-- Main effect is Maybe and sub-effect is IO

-- newtype MaybeIO a = MaybeIO (IO (Maybe a))
newtype MaybeIO a = MaybeIO {runMaybeIO :: IO (Maybe a)} 

    
instance Functor MaybeIO where
  -- fmap :: (a -> b) -> MaybeIO a -> MaybeIO b 
  fmap g (MaybeIO ioma) = MaybeIO $ do ma <- ioma
                                       case ma of 
                                        Nothing -> return Nothing
                                        Just a ->  return $ Just (g a) 


instance Applicative MaybeIO where
  -- pure :: a -> f a 
    pure = return 

  -- <*> :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b
  -- iomf :: IO (Maybe (a -> b)) ; ioma :: IO (Maybe a)
    MaybeIO iomf <*> MaybeIO ioma = MaybeIO $ do  mf <- iomf
                                                  case mf of
                                                    Nothing -> return Nothing
                                                    Just f -> do ma <- ioma
                                                                 case ma of
                                                                  Nothing -> return Nothing
                                                                  Just a -> return $ Just (f a)  

-- Make MaybeIO an instance of the monad class
instance Monad MaybeIO where
    -- return :: a -> Maybe IO a 
    -- return a = MaybeIO (return (Just a))
    return  =  MaybeIO .return. Just 

    -- (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    MaybeIO ioma >>= f = MaybeIO $ do ma <- ioma
                                      case ma of
                                        Nothing -> return Nothing
                                        Just a -> runMaybeIO (f a)     

    -- This wouldn't work since `ioma` actual type is `IO (Maybe a)` with expected type `MaybeIO (Maybe a)`
    -- couldn't match type `IO` with `MaybeIO`
    -- MaybeIO ioma >>= f =  do ma <- ioma
    --                          case ma of
    --                             Nothing -> MaybeIO $ return Nothing
    --                             Just a -> f a     


-- newtype MaybeIO a = MaybeIO (IO (Maybe a))
-- Just :: a -> Maybe a
-- fmap :: (a -> Maybe a) -> IO a -> IO (Maybe a)
-- fmap :: Functor f => (a -> b) -> f a -> f b
liftIOToMaybeIO :: IO a -> MaybeIO a
-- liftIOToMaybeIO n =  MaybeIO ( fmap (Just) n)           
liftIOToMaybeIO = MaybeIO . fmap Just 


-- readMaybe :: Read a => String -> Maybe a 
-- getLine :: IO String
readLine :: Read a => MaybeIO a
readLine = do  
              str <- liftIOToMaybeIO getLine
              MaybeIO $ return $ readMaybe str 


computation :: MaybeIO ()
computation = do a <- readLine 
                 b <- readLine
                 liftIOToMaybeIO $ print $ show (a + b)   
