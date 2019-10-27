CounterRep = {x: Ref Nat};
InstrCounterRep = {x: Ref Nat, a: Ref Nat};

SetCounter = {get: Unit -> Nat, set: Nat -> Unit, inc: Unit -> Unit};
InstrCounter = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit,
  accesses: Unit -> Nat
};

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

instrCounterClass =
  lambda r: InstrCounterRep.
    lambda self: Ref InstrCounter.
      let super = setCounterClass r self in
        {
          get = super.get,
          set = super.set,
          inc = super.inc,
          accesses = lambda _: Unit. !(r.a)
        };

sc = newSetCounter unit;
sc.inc unit;
sc.inc unit;
sc.get unit;
