open Util

type term =
  | Variable of {annot: annot; index: int; len_ctx: int}
      (** [annot] is for debugging. [index] is de Bruijn index of the
     variable. [len_ctx] is used to check consistency. it contains the
     total length of the context in which the variable occurs.*)
  | Abstraction of {annot: annot; binder: string; body: term}
  | Application of {annot: annot; lhs: term; rhs: term}

(** For the moment, the bindings themselves are completely trivial,
   carring no interesting information. Later on (in Simply Typed
   Lambda-Calculus), we'll introduce other clauses of the [binding]
   type that will keep track of the type assumptions associated with
   variables and other similar information.*)
type binding = NameBind

(** Just a list of strings and associated [binding]. *)
type context = (string * binding) list

type command = Eval of annot * term | Bind of annot * string * binding

(** functions *)

let annot_of = function
  | Variable {annot; _} | Abstraction {annot; _} | Application {annot; _} ->
      annot


(** functions for pretty-printing *)
let length_of (ctx : context) = List.length ctx

let bind : context -> string -> binding -> context = fun ctx x binding -> (x, binding) :: ctx

let find_map ~on_nth annot ctx idx =
  try on_nth (List.nth ctx idx)
  with Failure _ ->
    let msg = F.sprintf "Failed to lookup variable: offset %d, ctx size %d" idx (length_of ctx) in
    Error.error_at annot msg


let name_of_index annot ctx idx = find_map ~on_nth:fst annot ctx idx

let add_name ctx x = bind ctx x NameBind

let index_of_name annot ctx x =
  let rec loop ctx x acc =
    match ctx with
    | [] ->
        let msg = F.sprintf "Variable %s is unbound" x in
        Error.error_at annot msg
    | (x', _) :: ctx' ->
        if x = x' then acc else loop ctx' x (acc + 1)
  in
  loop ctx x 0


let is_bound ctx x = List.mem_assoc x ctx

(** When the printing routine needs to generate a fresh name for a
   bound variable, it tries first to use the supplied hint (the
   [binder] in [Abstraction]); if this turns out to clash with a name
   already used in the current context, it tries similar names, adding
   primes(') until it finds one that is not currently being used. This
   ensures that the printed term will be similar to what the user
   expects, modulo a few primes. *)
let rec pick_fresh ctx x = if is_bound ctx x then pick_fresh ctx (x ^ "'") else (add_name ctx x, x)

let pp_context fmt (ctx : context) =
  (* Since [binding] is trivial, print the variables only.*)
  let pp_item fmt item =
    let var = fst item in
    F.fprintf fmt "%s" var
  in
  Pp.pp_list ~pp_elt:pp_item fmt ctx


let lambda_char = "λ"

let is_small = function Variable _ -> true | Abstraction _ | Application _ -> false

let rec pp_term fmt ~ctx term =
  match term with
  | Abstraction {binder; body; _} ->
      let ctx, x = pick_fresh ctx binder in
      F.fprintf fmt "%s %s. %a" lambda_char x (pp_term ~ctx) body
  | Variable _ | Application _ ->
      pp_term_app fmt ~ctx term


and pp_term_app fmt ~ctx term =
  match term with
  | Application {lhs; rhs; _} ->
      F.fprintf fmt "@[<hov 0>%a %a@]" (pp_term_app ~ctx) lhs (pp_term_app ~ctx) rhs
  | Abstraction _ | Variable _ ->
      pp_atomic_term fmt ~ctx term


and pp_atomic_term fmt ~ctx term =
  match term with
  | Variable {index; len_ctx; annot} ->
      if length_of ctx <> len_ctx then
        let msg = F.asprintf "bad index: %i / %i in %a" index len_ctx pp_context ctx in
        Error.error_at annot msg
      else F.fprintf fmt "%s" (name_of_index annot ctx index)
  | Abstraction _ | Application _ ->
      F.fprintf fmt "(%a)" (pp_term ~ctx) term


let rec pp_de_bruijn fmt term =
  match term with
  | Abstraction {body; _} ->
      F.fprintf fmt "%s. %a" lambda_char pp_de_bruijn body
  | Variable _ | Application _ ->
      pp_de_bruijn_app fmt term


and pp_de_bruijn_app fmt term =
  match term with
  | Application {lhs; rhs; _} ->
      let pp = pp_de_bruijn_app in
      F.fprintf fmt "@[<hov 0>%a %a@]" pp lhs pp rhs
  | Abstraction _ | Variable _ ->
      pp_de_bruijn_atomic fmt term


and pp_de_bruijn_atomic fmt term =
  match term with
  | Variable {index; _} ->
      F.fprintf fmt "%i" index
  | Abstraction _ | Application _ ->
      F.fprintf fmt "(%a)" pp_de_bruijn term
