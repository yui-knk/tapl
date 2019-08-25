open Printf

type counter = {
  get: unit-> int;
  inc: unit -> unit;
};;

type resetCounter = {
  get: unit-> int;
  inc: unit -> unit;
  reset: unit -> unit;
};;

let newCounter =
  fun u -> let x = ref 1 in
    {
      get = (fun () -> !x);
      inc = (fun () -> x := !x + 1);
    };;

let newResetCounter =
  fun u -> let x = ref 1 in
    {
      get = (fun () -> !x);
      inc = (fun () -> x := !x + 1);
      reset = (fun () -> x := 1)
    };;

let inc3 c = c.inc (); c.inc (); c.inc ();;

let c = newCounter ();;
let rc = newResetCounter ();;

(* inc3 c;; *)
inc3 rc;;

printf "%d : %d\n" (c.get ()) (rc.get ());;
