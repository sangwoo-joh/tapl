module P = Printf
module F = Format

exception Exit of int

type annot = Lexing.position

let dummy_annot = Lexing.dummy_pos

let pp_annot fmt annot =
  if annot = dummy_annot then F.fprintf fmt "<Unknown location>: "
  else
    match annot with
    | {pos_fname; pos_lnum; pos_bol; pos_cnum} ->
        F.fprintf fmt "%s:%i.%i: " pos_fname pos_lnum (pos_cnum - pos_bol)


type 'a with_annot = {annot: annot; value: 'a}

let print_annot annot = pp_annot F.std_formatter annot

module Error = struct
  exception ParsingError

  let error_finally exn f =
    f () ;
    raise exn


  let error exn s =
    F.fprintf F.err_formatter "Error: %s\n" s ;
    raise exn


  let error_at ?(exn = ParsingError) annot s =
    F.fprintf F.err_formatter "Error at %a: %s\n" pp_annot annot s ;
    raise exn


  let warning s = F.fprintf F.std_formatter "Warning: %s\n" s

  let warning_at annot s = F.fprintf F.std_formatter "Warning at %a: %s\n" pp_annot annot s
end
