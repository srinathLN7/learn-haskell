# Writer Monad 

An example of a monad that captures the idea of computations while producing log outputs on the side. Notice in the code and example outputs for `foo''` and `fooDo`, we need not explicitly keep track of the log outputs i.e. accumulator values unlike `foo` and `foo'`. It is automatically taken care when the `bindWriter` operator is defined and is wrapped by the `Writer` monad. 


### Output
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
