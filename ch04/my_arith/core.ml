open Syntax

exception NoRuleApplies

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
  | t1 when isval t -> t1
  | TmIf(t1, t2, t3) ->
      (
        let e1 = eval1 t1 in
        let e2 = eval1 t2 in
        let e3 = eval1 t3 in
        match (e1, e2, e3) with
        | (TmTrue, t2', _) -> t2'
        | (TmFalse, _, t3') -> t3'
        | _ -> TmIf(t1, t2, t3)
      )
  | TmSucc(t1) ->
      (
        let e1 = eval1 t1 in
        match e1 with
        | t1' when isnumericval t1' -> TmSucc(t1')
        | _ -> TmSucc(t1)
      )
  | TmPred(t1) ->
      (
        let e1 = eval1 t1 in
        match e1 with
        | TmZero -> TmZero
        | TmSucc(t1') when isnumericval t1' -> t1'
        | _ -> TmPred(t1)
      )
  | TmIsZero(t1) ->
      (
        let e1 = eval1 t1 in
        match e1 with
        | TmZero -> TmTrue
        | TmSucc(t1') when isnumericval t1' -> TmFalse
        | _ -> TmIsZero(t1)
      )
  | _ -> t

let rec eval t = eval1 t
