import Text.Read

main :: IO ()
main = putStrLn "Hello World!!"


-- Write a simple function that takes in a user input and displays that on screen
displayStr :: IO ()
displayStr = getLine >>= putStrLn 

-- Write a function that gets two strings from the user input and concats the user output 
-- Here we use the lambda notations.
concatStr :: IO ()
concatStr = getLine >>= \x -> 
            getLine >>= \y -> 
            putStrLn $ x ++ " "  ++ y     


 