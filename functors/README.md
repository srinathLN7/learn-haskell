# Functor
Since class `Functor` is already defined in the prelude module, we rewrite the same definition using class Functors to avoid ambiguous errors
Hence in this module `Functors === Functor` and `fmaps === fmap`


### Output

```
Prelude> :l functors.hs
[1 of 1] Compiling Functors         ( functors.hs, interpreted )
Ok, one module loaded.
*Functors> fmaps (+1) Nothing
Nothing
*Functors> fmaps (*2) (Just 3)
Just 6
*Functors> fmaps not (Just False)
Just True
*Functors> fmaps length (Leaf "abc")
Leaf 3
*Functors> fmaps even (Node (Leaf 1) (Leaf 2))
Node (Leaf False) (Leaf True)
*Functors> fmaps show (return True)
"True"
*Functors> inc (Just 1)
Just 2
*Functors> inc [1,2,3,4,5,6]
[2,3,4,5,6,7]
*Functors> inc (Node (Leaf 1) (Leaf 2))
Node (Leaf 2) (Leaf 3)
*Functors> 
```
