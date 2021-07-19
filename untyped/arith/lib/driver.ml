open Util
module S = Syntax
module E = Eval
module F = Format

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
  let lexbuf = Lexing.from_channel ~with_positions:true chan in
  let ast =
    try Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error -> Error.error_at (Lexer.annot_of lexbuf) "Parse error"
  in
  Parsing.clear_parser () ;
  close_in chan ;
  ast


let already_imported = ref ([] : string list)

let rec process_file file =
  if List.mem file !already_imported then ()
  else (
    already_imported := file :: !already_imported ;
    let cmds = parse_file file in
    List.iter process_command cmds )


and process_command = function
  | S.Import f ->
      process_file f
  | S.Eval (_, t) ->
      let t = E.eval t in
      F.fprintf F.std_formatter "@[<hv>%a@.@]" S.pp_term t ;
      ()
