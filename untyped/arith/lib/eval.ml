open Syntax

exception No_rule_applies

let rec is_numeric_value = function
  | Zero _ ->
      true
  | Succ (_, t') ->
      is_numeric_value t'
  | _ ->
      false


let rec eval0 = function
  | If (_, True _, t, _else) ->
      t
  | If (_, False _, _then, t) ->
      t
  | If (a, cond, then', else') ->
      let cond' = eval0 cond in
      If (a, cond', then', else')
  | Succ (a, t) ->
      let t' = eval0 t in
      Succ (a, t')
  | Pred (_, Zero _) ->
      Zero Util.dummy_annot
  | Pred (_, Succ (_, t)) when is_numeric_value t ->
      t
  | Pred (a, t) ->
      let t' = eval0 t in
      Pred (a, t')
  | IsZero (_, Zero _) ->
      True Util.dummy_annot
  | IsZero (_, Succ (_, t)) when is_numeric_value t ->
      False Util.dummy_annot
  | IsZero (a, t) ->
      let t' = eval0 t in
      IsZero (a, t')
  | _ ->
      raise No_rule_applies


let rec eval t =
  try
    let t' = eval0 t in
    eval t'
  with No_rule_applies -> t
