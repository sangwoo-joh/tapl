open Util

type term =
  | True of annot
  | False of annot
  | If of annot * term * term * term  (** cond, then, else *)
  | Zero of annot
  | Succ of annot * term
  | Pred of annot * term
  | IsZero of annot * term

let rec pp_term fmt term =
  match term with
  | True _ ->
      F.fprintf fmt "true"
  | False _ ->
      F.fprintf fmt "false"
  | If (_, tcond, tthen, telse) ->
      F.fprintf fmt "@[<hv>if (%a) then (%a) else (%a)@]" pp_term tcond pp_term tthen pp_term telse
  | Zero _ ->
      F.fprintf fmt "0"
  | Succ (_, t) ->
      F.fprintf fmt "(succ %a)" pp_term t
  | Pred (_, t) ->
      F.fprintf fmt "(pred %a)" pp_term t
  | IsZero (_, t) ->
      F.fprintf fmt "iszero %a" pp_term t


type command = Import of string | Eval of annot * term [@@deriving show]

let annot_of = function
  | True a | False a | Zero a ->
      a
  | If (a, _, _, _) | Succ (a, _) | Pred (a, _) | IsZero (a, _) ->
      a
