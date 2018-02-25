open Support.Error
open Core

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
  let lexbuf = Lexer.create infile pi in
  let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error ->
    err "Parse error" in
    (* error (Lexer.info lexbuf) "Parse error" in *)
  Parsing.clear_parser(); close_in pi; result

let process_file infile =
  parseFile infile

let main () =
  let infile = parseArgs() in
  let result = process_file infile in
  let _ = Syntax.printtm(eval result) in
  ()

let _ = main()
