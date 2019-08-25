type counter = {
  get: unit-> int;
  inc: unit -> unit;
};;

let c = 
  let x = ref 1 in
    {
      get = (fun u -> !x);
      inc = (fun u -> x := !x + 1);
    };;

c.inc ();;
c.inc ();;

print_int (c.get ());;
