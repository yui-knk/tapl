CounterRep = {x: Ref Nat};
BackupCounterRep = {x: Ref Nat, b: Ref Nat };

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

decCounterClass =
  lambda r: CounterRep.
    let super = resetCounterClass r in
      {
        get = super.get,
        inc = super.inc,
        reset = super.reset,
        dec = lambda _: Unit. r.x := pred(!(r.x))
      };

newdecCounter =
  lambda _: Unit. let r = {x = ref 1} in
    decCounterClass r;

backupCounterClass =
  lambda r: BackupCounterRep.
    let super = resetCounterClass r in
      {
        get = super.get,
        inc = super.inc,
        reset = lambda _: Unit. r.x := !(r.b),
        backup = lambda _: Unit. r.b := !(r.x)
      };

newbackupCounter =
  lambda _: Unit. let r = {x = ref 1, b = ref 1} in
    backupCounterClass r;

bc = newbackupCounter unit;
bc.get unit;
bc.inc unit;
bc.get unit;
bc.reset unit;
bc.get unit;
bc.inc unit;
bc.inc unit;
bc.get unit;
bc.backup unit;
bc.reset unit;
bc.get unit;
