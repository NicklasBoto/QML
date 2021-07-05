# QML

Trying to perform static analysis on quantum conditionals, in a strict linear type system.

Example syntax:

```rb
def hadamard (x : qubit) -> qubit
:= if° x
     then [-1] ~1 + ~0
     else      ~1 + ~0
end
```
