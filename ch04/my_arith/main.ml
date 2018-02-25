open Support.Error

let argDefs = []

let parseArgs () =
  let infile = ref (None : string option) in
  Arg.parse argDefs
    (fun s ->
      match !infile with
        Some(_) -> err "You must specify exactly one input file"
      | None -> infile := Some(s))
    "";
  match !infile with
      None -> err "You must specify an input file"
    | Some(s) -> s


let openfile infile =
  open_in infile

let parseFile infile =
  let pi = openfile infile in
  pi

let process_file infile =
  parseFile infile

let main () =
  let infile = parseArgs() in
  let _ = process_file infile in
  ()

let _ = main()
