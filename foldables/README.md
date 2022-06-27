# Foldable
One of the primary applications of monoids in Haksell is to combine all the values in a data structure to give a single value. 

### Output
```
Prelude> :l Foldable.hs
[1 of 2] Compiling Monoids          ( Monoids.hs, interpreted )
[2 of 2] Compiling Foldables        ( Foldable.hs, interpreted )
Ok, two modules loaded.
*Monoids.Foldables> Foldables.foldMap Add [1..10]
Add {a = 55}
*Monoids.Foldables> Foldables.foldMap Mult [1..10]
Mult {b = 3628800}
*Monoids.Foldables> a (Foldables.foldMap Add [1..10])
55
*Monoids.Foldables> b (Foldables.foldMap Mult [1..10])
3628800
*Monoids.Foldables> tree
Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
*Monoids.Foldables> foldr (+) 0 tree
6
*Monoids.Foldables> foldl (+) 0 tree
6
*Monoids.Foldables>  
```
