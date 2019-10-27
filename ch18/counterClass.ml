CounterRep = {x: Ref Nat};
BackupCounterRep = {x: Ref Nat, b: Ref Nat };
BackupCounter2Rep = {x: Ref Nat, b: Ref Nat, b2: Ref Nat };

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

backupCounter2Class =
  lambda r: BackupCounter2Rep.
    let super = backupCounterClass r in
      {
        get = super.get,
        inc = super.inc,
        reset = super.reset,
        backup = super.backup,
        reset2 = lambda _: Unit. r.x := !(r.b2),
        backup2 = lambda _: Unit. r.b2 := !(r.x)
      };

newbackupCounter2 =
  lambda _: Unit. let r = {x = ref 1, b = ref 1, b2 = ref 1} in
    backupCounter2Class r;

funnyBackupCounterClass =
  lambda r: BackupCounterRep.
    let super = backupCounterClass r in
      {
        get = super.get,
        inc = lambda _: Unit. (super.backup unit; super.inc unit),
        reset = super.reset,
        backup = super.backup
      };

newFunnyBackupCounter =
  lambda _: Unit. let r = {x = ref 1, b = ref 1} in
    funnyBackupCounterClass r;

SetCounter = {get: Unit -> Nat, set: Nat -> Unit, inc: Unit -> Unit};

setCounterClass =
  lambda r: CounterRep.
    fix
      (
        lambda self: SetCounter.
          {
            get = lambda _: Unit. !(r.x),
            set = lambda i: Nat. r.x := i,
            inc = lambda _: Unit. self.set (succ (self.get unit))
          }
      );

newSetCounter =
  lambda _: Unit. let r = {x = ref 1} in
    setCounterClass r;

setCounterClass2 =
  lambda r: CounterRep.
    lambda self: SetCounter.
      {
        get = lambda _: Unit. !(r.x),
        set = lambda i: Nat. r.x := i,
        inc = lambda _: Unit. self.set (succ (self.get unit))
      };

newSetCounter2 =
  lambda _: Unit.
    let r = {x = ref 1} in
      fix (setCounterClass2 r);

InstrCounter = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit,
  accesses: Unit -> Nat
};

InstrCounterRep = { x: Ref Nat, a: Ref Nat };

instrCounterClass =
  lambda r: InstrCounterRep.
    lambda self: InstrCounter.
      let super = setCounterClass2 r self in
        {
          get = super.get,
          set = lambda i: Nat. (r.a := succ (!(r.a)); super.set 1),
          inc = super.inc,
          accesses = lambda _: Unit. !(r.a)
        };

newInstrCounter =
  lambda _: Unit.
    let r = {x = ref 1, a = ref 0} in
      fix (instrCounterClass r);

setCounterClassWithThunk =
  lambda r: CounterRep.
    lambda self: Unit -> SetCounter.
      lambda _: Unit.
        {
          get = lambda _: Unit. !(r.x),
          set = lambda i: Nat. r.x := i,
          inc = lambda _: Unit. (self unit).set (succ ((self unit).get unit))
        };

newSetCounterClassWithThunk =
  lambda _: Unit.
    let r = {x = ref 1} in
      fix (setCounterClassWithThunk r) unit;

instrCounterClassWithThunk =
  lambda r: InstrCounterRep.
    lambda self: Unit -> InstrCounter.
      lambda _: Unit.
        let super = setCounterClassWithThunk r self unit in
          {
            get = super.get,
            set = lambda i: Nat. (r.a := succ (!(r.a)); super.set 1),
            inc = super.inc,
            accesses = lambda _: Unit. !(r.a)
          };

newInstrCounterWithThunk =
  lambda _: Unit.
    let r = {x = ref 1, a = ref 0} in
      fix (instrCounterClassWithThunk r) unit;


/* 18.11 */
ic = newInstrCounterWithThunk unit;
ic.inc unit;
ic.inc unit;
/* 3 */
ic.get unit;
ic.accesses unit;
