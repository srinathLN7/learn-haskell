module Line where 
import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
               return []
               else
                  do xs <- getLine'
                     return (x:xs)           


readLine :: IO String
readLine = readLine' []


readLine' :: String -> IO String
readLine' xs = do
  x <- getCh
  case x of
     '\n'   -> do putChar '\n'
                  return (reverse xs)
     '\DEL' -> do putStr "\ESC[1D \ESC[1D"
                  readLine' (tail xs)
     _      -> do putChar x
                  readLine' (x:xs)