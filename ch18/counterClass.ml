CounterRep = {x: Ref Nat};

counterClass =
  lambda r: CounterRep.
    {
      get = lambda _: Unit. !(r.x),
      inc = lambda _: Unit. r.x := succ(!(r.x))
    };

newCounter =
  lambda _: Unit. let r = {x = ref 1} in
    counterClass r;

resetCounterClass =
  lambda r: CounterRep.
    let super = counterClass r in
      {
        get = super.get,
        inc = super.inc,
        reset = lambda _: Unit. r.x := 1
      };

newResetCounter =
  lambda _: Unit. let r = {x = ref 1} in
    resetCounterClass r;

rc = newResetCounter unit;
rc.get unit;
rc.inc unit;
rc.get unit;
rc.reset unit;
rc.get unit;
