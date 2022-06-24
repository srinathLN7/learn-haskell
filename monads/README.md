# Monads
Monads reduce boilerplate code needed for common operations (such as dealing with undefined values or fallible functions, or encapsulating bookkeeping code). 
Haskell use monads to turn complicated sequences of functions into succinct pipelines that abstract away control flow, and side-effects


### Output

```
Prelude> :l Monads.hs 
[1 of 1] Compiling Monads           ( Monads.hs, interpreted )
Ok, one module loaded.
*Monads> eval (Div (Val 1) (Val 0))
*** Exception: divide by zero
*Monads> safeEval (Div (Val 1) (Val 0))
Nothing
*Monads> safeEval (Div (Val 1) (Val 1))
Just 1
*Monads> prodsM [1,2] [3,4]
[3,4,6,8]
*Monads> tree
Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
*Monads> fst (app (alabel tree) 0)
Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
*Monads> fst (app (mlabel tree) 0)
Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
*Monads> mapM' conv "1234"
Just [1,2,3,4]
*Monads> mapM' conv "123a"
Nothing
*Monads> filterM' (\ x -> [True, False]) [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
*Monads> join [[1,2],[3,4],[5,6,7]]
[1,2,3,4,5,6,7]
*Monads> join (Just (Just 1))
Just 1
*Monads> join (Just Nothing)
Nothing
*Monads> join Nothing
Nothing
*Monads> 
```


