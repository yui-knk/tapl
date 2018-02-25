open Printf

module Error = struct
exception Exit of int

let errf f =
  f();
  raise (Exit 1)

let err s =
  errf (fun() -> printf "Error: %s.\n" s)

end
