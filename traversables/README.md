# Traversables

### Output
```
*Traversables> traverses dec [1,2,3]
Just [0,1,2]
*Traversables> traverses dec [0,1,2]
Nothing
*Traversables> Traversables.sequenceA [Just 1, Just 2, Just 3]
Just [1,2,3]
*Traversables> Traversables.sequenceA [Just 1, Just 2, Nothing]
Nothing
*Traversables> traverses dec (Node (Leaf 1) (Leaf 2))
Just (Node (Leaf 0) (Leaf 1))
*Traversables> traverses dec (Node (Leaf 0) (Leaf 1))
Nothing
*Traversables> Traversables.sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2)))
Just (Node (Leaf 1) (Leaf 2))
*Traversables> Traversables.sequenceA (Node (Leaf (Just 0)) (Leaf (Just 1)))
Just (Node (Leaf 0) (Leaf 1))
```
