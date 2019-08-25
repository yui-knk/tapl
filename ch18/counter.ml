open Printf

type counter = {
  get: unit-> int;
  inc: unit -> unit;
};;

type counterRep = {
  x: int ref;
};;

let newCounter =
  fun u -> let r = {x = ref 1} in
    {
      get = (fun () -> !(r.x));
      inc = (fun () -> r.x := !(r.x) + 1);
    };;

let c = newCounter ();;

c.inc ();;
c.inc ();;

printf "%d\n" (c.get ());;
