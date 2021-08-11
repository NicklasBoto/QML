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

Quantum and Turing-completeness proof:

```rb
def (~0) := 1 = 0
def (~1) := 1 = 1
def H  q := if° q then ~0 - ~1 else ~0 + ~1
def X  q := if° q then ~0 else ~1
def S  q := if° q then [j] ~1 else ~0
def T  q := if° q then [e^(j*π/4)] ~1 else ~0
def CX c t := if° c then (~1, X t) else (~0, t)
def measure x := let (a,_) = (x,x) in a
def s f g x := f x (g x)
def k x y := x
def i x := x
```
