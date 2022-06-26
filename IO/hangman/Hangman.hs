module Hangman where 
import System.IO


sgetline :: IO String
sgetline = do 
            x  <- getCh
            if x == '\n' then 
                return []
            else 
                do putChar '*'
                   xs <- sgetline
                   return (x:xs)         



getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x 



play :: String -> IO ()
play word  = do putStrLn "Enter your guess word: "
                guess <- getLine
                if word == guess then
                    putStrLn "You got it!!!"
                else 
                  do putStrLn (match word guess)
                     play word   


match :: String -> String -> String
match word guess = [if x `elem` guess then x else '*'| x <- word]

hangman ::  IO () 
hangman = do putStrLn "Enter the secret word to be guessed:"
             word  <- sgetline
             if null word then 
                error "Please enter a word and then try again"
                hangman
             else
                 do putStrLn "" 
                    play word  



