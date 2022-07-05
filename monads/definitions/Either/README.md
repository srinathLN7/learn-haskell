# Either 

`data Either a b = Left a | Right b`

We capture the idea of writing a function that tries to parse three Strings as Ints. If all the Strings can be successfully parsed as Ints, then the numbers are added to get a string. If one of the parses fails then an error message is returned. Notice the use of the `readMaybe` and `readEither` functions in the code.


### Output

```
*Main> foo "1" "2" "3"
Right 6
*Main> foo "1" "2" "Three"
Left "Cannot parse Three as Int"
*Main> foo' "1" "2" "3"
Right 6
*Main> foo' "1" "2" "Three"
Left "Cannot parse Three as Int"
*Main> fooPrefix' "1" "2" "3"
Right 6
*Main> fooPrefix' "1" "2" "Three"
Left "Cannot parse Three as Int"
```
