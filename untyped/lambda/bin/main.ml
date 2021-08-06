open Lambda

let args =
  [ ( "-I"
    , Arg.String (fun file -> Driver.search_path := file :: !Driver.search_path)
    , "Append a directory to the search path" ) ]


let usage_msg = "[-I <dir>]* ... <file>"

let parse_args () =
  let file = ref (None : string option) in
  Arg.parse args
    (fun s ->
      match !file with
      | Some _ ->
          raise (Invalid_argument "You must specify exactly one input file.")
      | None ->
          file := Some s)
    usage_msg ;
  match !file with
  | None ->
      raise (Invalid_argument "You must specify at least one input file.")
  | Some s ->
      s


let main () =
  let file = parse_args () in
  ignore (Driver.process_file Lambda.Eval.empty_ctx file)


let () = main ()
