module Monads where
import Data.Char

--- Monads reduce boilerplate code needed for common operations (such as dealing with undefined values or fallible functions, or encapsulating bookkeeping code). 
--- Haskell use monads to turn complicated sequences of functions into succinct pipelines that abstract away control flow, and side-effects

---  Consider this example 

data Expr = Val Int | Div Expr Expr


-- Problem with this function is not it does not capture the failure (division by 0) without crashing the program
eval :: Expr -> Int
eval (Val n) = n 
eval (Div x y) = eval x `div` eval y 

-- To handle failure without crashing
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- This function returns "Nothing" when divided by zero
safeEval :: Expr -> Maybe Int
safeEval (Val n) = Just n
safeEval (Div x y) = case safeEval x of
                        Nothing -> Nothing
                        Just m -> case safeEval y of
                                    Nothing -> Nothing
                                    Just n -> safeDiv m n   


-- To abstract the above common pattern that repeats again and again, 
-- we define the bind operator which effectively takes the result of the first and binds it to the second argument
-- bindOp === (>>=) to avoid ambiguous error from Prelude

bindOp :: Maybe a -> (a -> Maybe b) -> Maybe b
mx `bindOp` f = case mx of
                    Nothing -> Nothing
                    Just x -> f x
--- === maybe Nothing f mx
 

-- safeEvalM defined using the bind operator (>>=)
safeEvalM :: Expr -> Maybe Int
safeEvalM (Val n) = Just n
safeEvalM (Div x y) = safeEvalM x >>= \m ->
                      safeEvalM y >>= \n ->
                      safeDiv m n 


-- To further simplify safeEvalM 
-- Haskell lets us employ the do notation
safeEvalM' :: Expr -> Maybe Int
safeEvalM' (Val n) = Just n
safeEvalM' (Div x y)  = do  m <- safeEvalM' x
                            n <- safeEvalM' y
                            safeDiv m n 

-- Equivalence of the two monadic notations
{- 
    m1 >> \x1 ->                            do  x1 <- m1
    m2 >> \x2 ->                                x2 <- m2    
    .                                           .
    .                           ======          .
    mn >> \xn ->                                xn <- mn     
    f x1 x2 .. xn                               f x1 x2 .. xn 
-}


-- Monad class definition - To avoid ambiguous error from Prelude module
--- Monads === Monad; returns === return; 
--- bindop === (>>=) 
class Applicative m => Monads m where
    returns :: a -> m a
    bindop :: m a -> (a -> m b) -> m b 

    returns = pure


 -- Maybe Monad declaration
instance Monads Maybe where
    -- bindop :: Maybe a -> (a -> Maybe b) -> Maybe b

   Nothing `bindop` _ = Nothing
   Just x  `bindop` f = f x


-- List Monad declaration 
instance Monads [] where
    -- bindop :: [a] -> (a -> [b]) -> [b]

    xs `bindop` f = [ y | x <- xs, y <- f x ]


-- compute the product of two list using monadic style compuation
-- prodsM === prodsM' 

prodsM :: Num a => [a] -> [a] -> [a]
prodsM xs ys = do  x <- xs 
                   y <- ys
                   returns (x*y)
  
prodsM' :: Num a => [a] -> [a] -> [a]
prodsM' xs ys = xs `bindop` \ x -> 
                ys `bindop` \ y ->
                returns (x*y)


---- THE STATE MONAD EXAMPLE

type State = Int 

-- To make a state transformer an instance of a monad class - we need to use data or newtype since `type` cannot be made into instances
newtype ST a = S (State -> (a, State))

-- char -> ST Int === char -> State -> (Int, State)

-- define a function app to extract the values alone from the state transformer 
app :: ST a -> State -> (a, State)
app (S st)  = st 

-- make ST an instance of Functor class 
instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f st =  S (\ s -> let (x, s') =  app st s in (f x, s')) 

-- fmap lets us apply the function 'f' to the result value of the state transformer 'st' with initial state s

-- make ST an instance of Applicative
instance Applicative ST where 
    -- pure :: a -> ST a
    pure x = S (\ s -> (x, s)) 

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s -> let  (f, s') = app stf s  
                                (x, s'') = app stx s'
                                in (f x , s'') ) 



-- make ST an instance of the Monad class

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b 
    stx >>= f = S (\ s -> let (x, s') = app stx s  
                               in app (f x) s')

--    stx >>= f = do  x <- stx
--                    f x  


-- Application of the STATE MONAD example
-- Relabelling trees with a unique integer at every leaf 

data Tree a = Leaf a | Node (Tree a) (Tree a)  deriving Show


-- We could define a pure function say rlabel :: Tree Int -> Int -> (Tree Int , Int)
-- A simpler way to do this is to use the state monads

-- newtype ST (Tree Int) = S (Int -> (Tree Int, Int))
-- rlabel :: Tree Int -> ST (Tree Int)


-- fresh returns the current state as the result value and updates the state by incrementing the current state
fresh :: ST Int 
fresh = S (\n -> (n , n+1))

-- ST is an applicative functor
-- <*> :: ST (a -> b) -> ST a -> ST b

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r    


-- ST is an monad
-- (>>=) :: ST a -> (a -> ST b) -> ST b
-- Leaf :: a -> Tree a 
-- Node :: Tree a -> Tree a -> Tree a
-- (>>=) :: ST a -> (a -> ST (Tree Int)) -> ST (Tree Int)    

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do  n <- fresh
                      return (Leaf n)

mlabel (Node l r) = do  l' <- mlabel l
                        r' <- mlabel r
                        return (Node l' r')                      


-- To test the above, let us define an example tree

tree :: Tree Char 
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')


--- GENERIC MONADIC FUNCTIONS

-- mapM === mapM' 

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' mf (x:xs) = do  y <- mf x
                      ys <- mapM' mf xs
                      return (y:ys)    


conv :: Char -> Maybe Int
conv ch | isDigit ch = Just (digitToInt ch)
        | otherwise = Nothing    



filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' mp (x:xs) = do p <- mp x
                        ys <- filterM' mp xs
                        return (if p then x:ys else ys)


join :: Monad m => m (m a) -> m a
join mmx = do  mx <- mmx
               x  <- mx
               return x




----- MONAD LAWS 

-- return x >>= f = f x 
-- mx >>= return = m x  
-- (mx >>= f) >>= g = mx >>= (\ x -> (f x >>= g)) 
