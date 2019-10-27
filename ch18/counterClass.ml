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

InstrCounter2 = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit,
  accesses_a: Unit -> Nat,
  accesses_b: Unit -> Nat
};

InstrCounterRep = { x: Ref Nat, a: Ref Nat };
/* a is "set" called count, b is "get" called count */
InstrCounterRep2 = { x: Ref Nat, a: Ref Nat, b: Ref Nat };

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

instrCounterClass2WithThunk =
  lambda r: InstrCounterRep2.
    lambda self: Unit -> InstrCounter2.
      lambda _: Unit.
        let super = setCounterClassWithThunk r self unit in
          {
            get = lambda _: Unit. (r.b := succ (!(r.b)); super.get unit),
            set = lambda i: Nat. (r.a := succ (!(r.a)); super.set 1),
            inc = super.inc,
            accesses_a = lambda _: Unit. !(r.a),
            accesses_b = lambda _: Unit. !(r.b)
          };

newInstrCounter2WithThunk =
  lambda _: Unit.
    let r = {x = ref 1, a = ref 0, b = ref 0} in
      fix (instrCounterClass2WithThunk r) unit;


/* 18.11.1 (1) */
ic2 = newInstrCounter2WithThunk unit;
ic2.inc unit;
ic2.inc unit;
/* 3 */
ic2.get unit;
ic2.accesses_a unit;
ic2.accesses_b unit;
