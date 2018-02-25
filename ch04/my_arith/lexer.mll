{

let create infile stream =
  Lexing.from_channel stream

}

let space = [' ' '\t' '\n' '\r']

rule main = parse
| space+
  { main lexbuf }
| "if"
  { Parser.IF }
| "then"
  { Parser.THEN }
| "else"
  { Parser.ELSE }
| "true"
  { Parser.TRUE }
| "false"
  { Parser.FALSE }
| "succ"
  { Parser.SUCC }
| "pred"
  { Parser.PRED }
| "iszero"
  { Parser.ISZERO }
| '0'
  { Parser.ZERO }
| eof
  { Parser.EOF }
| _
    { failwith
  (Printf.sprintf "unknown token %s near characters %d-%d"
     (Lexing.lexeme lexbuf)
     (Lexing.lexeme_start lexbuf)
     (Lexing.lexeme_end lexbuf)) }
