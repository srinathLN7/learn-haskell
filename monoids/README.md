# Monoids

A monoid is a set together with an associative opeartor that cobines two elements from the set, and an identity element for the operator. 

In mathematics, a monoid is a semi-group with an identity element.  A semi-group is an associative Magma.
A magma represents a set `S` with closed binary opeartions i.e. the resulting operations produce the results in the same set `S`. 

### Output 
```
Prelude> :l Monoid.hs 
[1 of 1] Compiling Monoids          ( Monoid.hs, interpreted )
Ok, one module loaded.
*Monoids> mconcats [Add 2, Add 3, Add 4]
Add {a = 9}
*Monoids> mconcats [Mult 2, Mult 3, Mult 4]
Mult {b = 24}
*Monoids> mconcats [All True, All True, All True]
All {c = True}
*Monoids> mconcats [All True, All True, All False]
All {c = False}
*Monoids> mconcats [Any False, Any False, Any True]
Any {d = True}
*Monoids> mconcats [Any False, Any False, Any False]
Any {d = False}
*Monoids> 
```
