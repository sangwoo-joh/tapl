open Util
module S = Syntax
module E = Eval

let search_path = ref [""]

let open_file file =
  let rec try_next = function
    | [] ->
        Error.error Not_found ("Could not find" ^ file)
    | dir :: rest -> (
        let name = if dir = "" then file else dir ^ "/" ^ file in
        try open_in name with Sys_error _ -> try_next rest )
  in
  try_next !search_path


let parse_file file =
  let chan = open_file file in
  let lexbuf = Lexer.ready file chan in
  let ast =
    try Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error -> Error.error_at (Lexer.annot_of lexbuf) "Parse error"
  in
  Parsing.clear_parser () ;
  close_in chan ;
  ast


let already_imported = ref ([] : string list)

let process_command ctx cmd =
  match cmd with
  | S.Eval (_, t) ->
      let t = E.eval ctx t in
      F.fprintf F.std_formatter "@[<hv>%a@]" (S.pp_term ~ctx) t ;
      ctx
  | S.Bind (_, x, binding) ->
      S.bind ctx x binding


let process_file ctx file =
  already_imported := file :: !already_imported ;
  let cmds, _ctx = (parse_file file) ctx in
  List.fold_left process_command ctx cmds
