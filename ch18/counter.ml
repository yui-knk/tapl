type counter = {
  get: unit-> int;
  inc: unit -> unit;
};;

let newCounter =
  fun u -> let x = ref 1 in
    {
      get = (fun u -> !x);
      inc = (fun u -> x := !x + 1);
    };;

let c = newCounter ();;

c.inc ();;
c.inc ();;
print_int (c.get ());;
