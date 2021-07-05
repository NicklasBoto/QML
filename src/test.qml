def pauliZ (x : qubit) -> qubit
:=  if° x
      then [-1] ~1
      else      ~0
end
 
def hadamard (x : qubit) -> qubit
:= if° x
     then [-1] ~1 + ~0
     else      ~1 + ~0
end

def pauliX (x : qubit) -> qubit
:= if° x
     then ~0
     else ~1
end

def tup -> qubit := (~0,~0) end

tup = (~0,~0)

-- def cnot (c : qubit, t : qubit) -> qubit * qubit
-- :=  if° c
--       then (~1, qnot t)
--       else (~0,      t)
-- end

-- def tel (x : qubit) -> qubit
-- :=  let (a,b)   = (~0, ~0) + (~1, ~1)
--         (a',b') = cnot a x
--         b'      = if a' then qnot b else b
--         b''     = if° had x' then pZed b' else b'
--     in b''
-- end