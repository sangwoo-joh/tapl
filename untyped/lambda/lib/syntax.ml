open Util

type term =
  | Variable of {annot: annot; index: int; len_ctx: int}
  | Abstraction of {annot: annot; binder: string; body: term}
  | Application of {annot: annot; lhs: term; rhs: term}

type binding = NameBind

type context = (string * binding) list

type command = Eval of annot * term | Bind of annot * string * binding
