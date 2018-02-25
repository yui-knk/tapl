open Syntax

exception NoRuleApplies

type result =
    S of term
  | E of term

let rec isnumericval t =
  match t with
  | TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false

let rec isval t =
  match t with
  | TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

let rec eval1 t =
  match t with
  | TmIf(TmTrue, t2, t3) -> S(t2)
  | TmIf(TmFalse, t2, t3) -> S(t3)
  | TmIf(t1, t2, t3) ->
      (
        let t' = eval1 t1 in
        match t' with
        | S(t1') -> S(TmIf(t1', t2, t3))
        | E(_) -> E(TmIf(t1, t2, t3))
      )
  | TmSucc(t1) ->
      (
        let t' = eval1 t1 in
        match t' with
        | S(t1') -> S(TmSucc(t1'))
        | E(_) -> E(TmSucc(t1))
      )
  | TmPred(TmZero) -> S(TmTrue)
  | TmPred(TmSucc(nv1)) when isnumericval nv1 -> S(nv1)
  | TmPred(t1) ->
      (
        let t' = eval1 t1 in
        match t' with
        | S(t1') -> S(TmPred(t1'))
        | E(_) -> E(TmPred(t1))
      )
  | TmIsZero(TmZero) -> S(TmTrue)
  | TmIsZero(TmSucc(nv1)) when isnumericval nv1 -> S(TmFalse)
  | TmIsZero(t1) ->
      (
        let t' = eval1 t1 in
        match t' with
        | S(t1') -> S(TmIsZero(t1'))
        | E(_) -> E(TmIsZero(t1))
      )
  | _ -> E(t)

let rec eval t =
  let t' = eval1 t in
  match t' with
  | S(t2) -> eval t2
  | E(t2) -> t2
