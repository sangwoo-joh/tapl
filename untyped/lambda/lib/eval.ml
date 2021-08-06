open Syntax

let empty_ctx : context = []

let get_binding annot ctx idx = find_map ~on_nth:snd annot ctx idx

let shift diff term =
  let rec walk cutoff term =
    match term with
    | Variable ({index; _} as var) ->
        (* Note that in both cases, we update the total length of
           context to check consistency later.*)
        let len_ctx = var.len_ctx + diff in
        if index >= cutoff then Variable {var with len_ctx; index= var.index + diff}
        else Variable {var with len_ctx}
    | Abstraction abs ->
        Abstraction {abs with body= walk (cutoff + 1) abs.body}
    | Application app ->
        Application {app with lhs= walk cutoff app.lhs; rhs= walk cutoff app.rhs}
  in
  walk 0 term


(** The only slightly subtle point is that reducing a redex "uses up"
   the bound variable: when we reduce ((\x.t) v) to [x|->v] t, the
   bound variable x disappears in the process. Thus, we will need to
   renumber the variables of the result of substitution to take into
   account the fact that x is no longer part of the context. For
   example, (\.1 0 2) (\.0) --> 0 (\.0) 1, not 1 (\.0) 2.

   Similarly, we need to shift the variables in v up by one before
   substituting into t, since t is defined in a larger context than v.

   The only difference from this original definition of substitution
   is that here we do all the shifting of v at once, in [Variable]
   case, rather than shifting v up by one every time we go through a
   binder. This means that the argument x is the same in every call to
   [walk], and we can omit it from the inner definition.  *)
let substitute free replacement term =
  let rec walk cutoff term =
    match term with
    | Variable {index; _} as var ->
        if index = free + cutoff then shift cutoff replacement else var
    | Abstraction abs ->
        Abstraction {abs with body= walk (cutoff + 1) abs.body}
    | Application app ->
        Application {app with lhs= walk cutoff app.lhs; rhs= walk cutoff app.rhs}
  in
  walk 0 term


(** In the operational semantics of the lambda-calculus, the only
   place where substitution is used is in the beta-reduction rule. As
   we noted before, this rule actually performs several operations:
   the term being substituted for the bound variable is first shifted
   up by one, then the substitution is made, and then the whole result
   is shifted down by one to account for the fact that the bound
   variable has been used up.*)
let beta_reduction s t = shift (-1) (substitute 0 (shift 1 s) t)

(* evaluations *)
let is_value = function Abstraction _ -> true | Application _ | Variable _ -> false

exception No_rule_applies

let rec eval0 ctx term =
  match term with
  | Application {lhs= Abstraction {body; _}; rhs; _} when is_value rhs ->
      (* beta-reduction for (λ.body) rhs *)
      beta_reduction rhs body
  | Application ({lhs; rhs; _} as app) when is_value lhs ->
      let rhs = eval0 ctx rhs in
      Application {app with rhs}
  | Application ({lhs; _} as app) ->
      let lhs = eval0 ctx lhs in
      Application {app with lhs}
  | _ ->
      raise No_rule_applies


let rec eval ctx term =
  try
    let term = eval0 ctx term in
    eval ctx term
  with No_rule_applies -> term
