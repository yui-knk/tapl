CounterRep = {x: Ref Nat};
SetCounter = {get: Unit -> Nat, set: Nat -> Unit, inc: Unit -> Unit};

setCounterClass =
  lambda r: CounterRep.
    lambda self: Ref SetCounter.
      {
        get = lambda _: Unit. !(r.x),
        set = lambda i: Nat. r.x := i,
        inc = lambda _: Unit. (!self).set (succ ((!self).get unit))
      };

dummySetCounter =
 {
   get = lambda _: Unit. 0,
   set = lambda _: Nat. unit,
   inc = lambda _: Unit. unit
 };

newSetCounter =
  lambda _: Unit.
    let r = {x = ref 1} in
    let cAux = ref dummySetCounter in
    (cAux := setCounterClass r cAux; !cAux);

sc = newSetCounter unit;
sc.inc unit;
sc.inc unit;
sc.get unit;
