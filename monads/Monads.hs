module Monads where

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