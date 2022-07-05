# Writer Monad 

```
*Main> :l Writer.hs 
[1 of 1] Compiling Main             ( Writer.hs, interpreted )
Ok, one module loaded.
*Main> x
Writer 6 ["number: 1","number: 2","number: 3"]
*Main> y
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
*Main> z
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
*Main> z'
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
```
