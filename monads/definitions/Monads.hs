import Text.Read
import Control.Monad

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int 
threeInts mx my mz = mx >>= \k ->
                     my >>= \l ->
                     mz >>= \m ->
                     let s = k + l + m in return s             


threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int 
threeInts' mx my mz =  do 
                       k <- mx
                       l <- my
                       m <- mz 
                       let s = k+l+m   -- no in is used here
                       return s        

foo :: String -> String -> String -> Maybe Int
foo x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)      

foo' :: String -> String -> String -> Either String Int
foo' x y z = threeInts (readEither x) (readEither y) (readEither z)

data Writer a = Writer a [String] deriving Show

 