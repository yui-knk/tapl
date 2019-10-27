ff = lambda ie: Nat -> Bool.
  lambda x: Nat.
    if iszero x then true
    else if iszero (pred x) then false
    else ie (pred (pred x));

iseven = fix ff;

iseven 7;
iseven 8;
