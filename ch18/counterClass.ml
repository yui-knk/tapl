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

/* 18.7.1 */
fb = newFunnyBackupCounter unit;
/* b = 1 */
fb.inc unit;
/* b = 2 */
fb.inc unit;
/* b = 3 */
fb.inc unit;
fb.get unit;
fb.reset unit;
fb.get unit;
