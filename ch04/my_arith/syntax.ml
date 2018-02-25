open Printf

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec printtm t =
  match t with
  | TmTrue -> printf "true"
  | TmFalse -> printf "false"
  | TmIf(t1, t2, t3) -> printf "if "; printtm t1; printf " then "; printtm t2; printf " else "; printtm t3
  | TmZero -> printf "0"
  | TmSucc(t1) -> printf "succ "; printtm t1
  | TmPred(t1) -> printf "pred "; printtm t1
  | TmIsZero(t1) -> printf "iszero "; printtm t1
