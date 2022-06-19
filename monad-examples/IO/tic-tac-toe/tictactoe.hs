module Tictactoe where

import Data.Char
import Data.List
import System.IO 

--- GRID UTILITIES

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)


full :: Grid -> Bool
full = notElem B . concat 


turn :: Grid -> Player
turn gs = if os <= xs then O else X
             where
                g = concat gs
                os = length $ filter (== O) g 
                xs = length $ filter (== X) g     



wins :: Grid -> Player -> Bool 
wins gs p = any line (rows ++ cols ++ diags)
            where 
                line = all (== p)
                rows = gs
                cols = transpose gs
                diags = [diag gs , diag (map reverse gs)]
 


diag :: Grid -> [Player]
diag gs = [gs !! n !! n | n <- [0.. size-1]]

won :: Grid -> Bool
won gs = wins gs O || wins gs X



-- Displaying the GRID

putGrid :: Grid -> IO ()
putGrid =  putStrLn . unlines . concat . interleave bar . map showRow
            where bar = [replicate ((size*4) -1) '-'] 


showRow :: [Player] -> [String]
showRow  = beside. interleave bar . map showPlayer
           where 
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|" 

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]


interleave :: a -> [a] -> [a]
interleave x []  = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys 

-- MAKE A MOVE IN THE GRID

valid :: Grid -> Int -> Bool
valid gs index = index >= 0 && index < size^2 && (concat gs !! index == B)

move :: Grid -> Player -> Int -> [Grid]
move gs p i = [chop size (xs ++ [p] ++ ys)| valid gs i]  
              where (xs, B:ys) = splitAt i (concat gs)  


chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)



-- PLAY THE GAME

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs  <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else 
                     do putStrLn "ERROR: Invalid Number. Please try again!"
                        getNat prompt

type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

newline :: IO ()
newline = putChar '\n'

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


-- USE MUTUAL RECURSION

promptStr :: Player -> String
promptStr p = "Player " ++ show p ++ " >> Enter your move: " 

run :: Grid -> Player -> IO ()
run gs p = do cls
              goto (1,1)
              newline
              putGrid gs
              run' gs p
                
 

run' :: Grid -> Player -> IO ()
run' gs p   | wins gs X = putStrLn "Player X wins!!!"
            | wins gs O = putStrLn "Player O wins!!!"
            | full gs = putStrLn "It is a draw!!!"
            | otherwise = do index <- getNat (promptStr p)
                             case move gs p index of 
                                 []    -> do putStrLn "ERROR!!! Invalid move. Please try again"
                                             run' gs p
                                 [gs'] -> run gs' (next p)
                                 _     -> run' gs p           




tictactoe :: IO ()
tictactoe = do putStr "Who wants to go first ? Player O or Player X? "
               p <- getChar 
               case toUpper p of 
                'O' -> run empty O
                'X' -> run empty X
                _ -> do putStrLn "Invalid player name. Please enter again"
                        tictactoe   
                 