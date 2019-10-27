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
    lambda self: Source SetCounter.
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
    lambda self: Source InstrCounter.
      let super = setCounterClass r self in
        {
          get = super.get,
          set = lambda i: Nat. (r.a := succ (!(r.a)); super.set i),
          inc = super.inc,
          accesses = lambda _: Unit. !(r.a)
        };

dummyInstrCounter =
  {
    get = lambda _: Unit. 0,
    set = lambda _: Nat. unit,
    inc = lambda _: Unit. unit,
    accesses = lambda _: Unit. 0
  };

newInstrCounterClass =
  lambda _: Unit.
    let r = {x = ref 1, a = ref 0} in
    let cAux = ref dummyInstrCounter in
    (cAux := instrCounterClass r cAux; !cAux);

sc = newSetCounter unit;
sc.inc unit;
sc.inc unit;
sc.get unit;

ic = newInstrCounterClass unit;
ic.inc unit;
ic.inc unit;
ic.get unit;
ic.accesses unit;
