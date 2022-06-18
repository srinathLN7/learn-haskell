module Nim where

import Data.Char    

-- GAME UTILITIES

type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1

initial :: Board
initial = [5,4,3,2,1]

finished:: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num =  board !! (row -1) >= num  

move :: Board -> Int -> Int -> Board 
move board row num = [if row == r then n - num else n |(r,n) <- zip [1..] board]


-- IO UTILITIES

putRow :: Int -> Int -> IO ()
putRow row num = do putChar '\n'
                    putStr (show row ++ ": ")
                    print ( concat $ replicate num "*")


-- sequence_ :: [IO a] -> IO ()

putBoard :: Board -> IO ()
putBoard board = sequence_ [ putRow r b | (r,b) <- zip [1..] board]


newline :: IO ()
newline = putChar '\n'


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     if isDigit x then 
                        return (digitToInt x)
                     else 
                       do putStrLn "Invalid digit"
                          getDigit prompt           



--  MAIN GAME FUNCTION

play :: Board -> Int -> IO ()
play board player = do putBoard board
                       newline 
                       if finished board then
                           putStrLn ("Player " ++ show (next player) ++ " wins!!!")
                       else 
                           do print ("Playing now --> Player:" ++ show player)
                              newline
                              row <- getDigit "Enter row number: "
                              newline   
                              num <- getDigit "Enter number of stars to remove: "
                              newline  
                              if valid board row num then
                                play (move board row num) (next player)
                              else
                                  do putStrLn "ERROR: INVALID MOVE!!! Please try again"
                                     play board player                 


nim :: IO ()
nim = play initial 1