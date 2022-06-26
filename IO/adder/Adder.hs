module Adder where

triggeradder :: Int -> Int -> IO Int
triggeradder total 0 = return total
triggeradder total n = do 
                        arg <- getLine
                        triggeradder (total +  read arg :: Int) (n-1)



adder :: IO ()
adder = do 
        putStr  "How many numbers?"
        n <- getLine
        result <- triggeradder 0 (read n :: Int)
        print ("The total is " ++ show result) 


                    
-- sequence :: [IO a] -> IO [a]

getNum :: IO Int 
getNum  = do 
            num <- getLine 
            return (read num :: Int)


adder' :: IO () 
adder' = do 
         putStr  "How many numbers?"
         num <- getLine 
         args <- sequence (replicate (read num :: Int) getNum)
         print ("The total is " ++ show (sum args) )  