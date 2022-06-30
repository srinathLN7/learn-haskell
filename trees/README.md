# Output

```
*Trees> tree1
Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)
*Trees> tree2
Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf)
*Trees> foldr (+) 0 tree1
6
*Trees> foldr (+) 0 tree2
3
*Trees> foldl (+) 0 tree1
6
*Trees> foldl (+) 0 tree2
3
*Trees> traverse dec tree1
Just (Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf))
*Trees> traverse dec tree2
Nothing
```
