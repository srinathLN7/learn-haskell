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

# Applicative
Since class `Applicative` is already defined in the prelude module, we rewrite the same definition using class Applicative to avoid ambiguous errors.
Hence in this module `Applicatives === Applicative`  

### Output
```
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude> :l Applicatives.hs 
[1 of 2] Compiling Functors         ( Functors.hs, interpreted )
[2 of 2] Compiling Applicatives     ( Applicatives.hs, interpreted )
Ok, two modules loaded.
*Applicatives> Applicatives.pure (+1)  Applicatives.<*> Just 1
Just 2
*Applicatives> Applicatives.pure (+)  Applicatives.<*> Just 1 Applicatives.<*> Just 2
Just 3
*Applicatives> Applicatives.pure (+)  Applicatives.<*> Nothing  Applicatives.<*> Just 2
Nothing
*Applicatives> Applicatives.pure (+1)  Applicatives.<*> [1,2,3]
[2,3,4]
*Applicatives> Applicatives.pure (+)  Applicatives.<*> [1] Applicatives.<*> [2]
[3]
*Applicatives> Applicatives.pure (*)  Applicatives.<*> [1,2] Applicatives.<*> [3,4]
[3,4,6,8]
*Applicatives> prods [1,2] [3,4]
[3,4,6,8]
*Applicatives> getChars 7
srinath"srinath"
*Applicatives> exA
Nothing
*Applicatives> exB
Nothing
*Applicatives> exC
Nothing
*Applicatives> exD
Just (Employee {name = "Srinath", phone = "0021"})
*Applicatives> employees
[Employee {name = "Jones", phone = "001"},Employee {name = "Jones", phone = "002"},Employee {name = "Jones", phone = "003"},Employee {name = "Sara", phone = "001"},Employee {name = "Sara", phone = "002"},Employee {name = "Sara", phone = "003"},Employee {name = "Ali", phone = "001"},Employee {name = "Ali", phone = "002"},Employee {name = "Ali", phone = "003"}] 
*Applicatives> employeesSpecific 
[Employee {name = "Jones", phone = "001"},Employee {name = "Sara", phone = "002"},Employee {name = "Ali", phone = "003"}]
```

