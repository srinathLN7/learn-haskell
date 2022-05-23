module MergeSort where

-- First, let us define the merge function that merges two sorted list to give a single sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys 
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys 

                    
-- halve spilits a given list in to a tuple with arity 2 where the number of components between the two differ by atmost 1.
halve :: [a] -> ([a],[a])
halve xs =  splitAt (length xs `div` 2) xs                         

-- MERGE SORT - Recurrsively sort the the two lists and merge these two sorted lists using the merge function
-- Adding condition on line 19 - makes the performance pretty paster 
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (halve xs))) (msort(snd (halve xs)))
