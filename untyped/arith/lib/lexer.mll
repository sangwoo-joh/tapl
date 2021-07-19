{
open Util
open Parser

let keyword_tbl = Hashtbl.create 31
let () = List.iter (fun (keyword, tok_gen) -> Hashtbl.add keyword_tbl keyword tok_gen)
    [
      (* Keywords *)
      ("import", fun a -> IMPORT a);
      ("if", fun a -> IF a);
      ("then", fun a -> THEN a);
      ("else", fun a -> ELSE a);
      ("true", fun a -> TRUE a);
      ("false", fun a -> FALSE a);
      ("succ", fun a -> SUCC a);
      ("pred", fun a -> PRED a);
      ("iszero", fun a -> ISZERO a);
      (* Symbols *)
      ("_", fun a -> UNDER_SCORE a);
      ("'", fun a -> APOSTROPHE a);
      ("\"", fun a -> DOUBLE_QUOTE a);
      ("!", fun a -> BANG a);
      ("#", fun a -> HASH a);
      ("$", fun a -> TRIANGLE a);
      ("*", fun a -> STAR a);
      ("|", fun a -> BAR a);
      (".", fun a -> DOT a);
      (",", fun a -> COMMA a);
      (";", fun a -> SEMICOLON a);
      (":", fun a -> COLON a);
      ("::", fun a -> DOUBLE_COLON a);
      ("/", fun a -> SLASH a);
      ("=", fun a -> EQ a);
      ("==", fun a -> DOUBLE_EQ a);
      ("[", fun a -> LEFT_SQUARE a);
      ("<", fun a -> LT a);
      ("{", fun a -> LEFT_CURLY a);
      ("(", fun a -> LEFT_PAREN a);
      ("<-", fun a -> LEFT_ARROW a);
      ("{|", fun a -> LEFT_CURLY_BAR a);
      ("[|", fun a -> LEFT_SQUARE_BAR a);
      ("]", fun a -> RIGHT_SQUARE a);
      (">", fun a -> GT a);
      ("}", fun a -> RIGHT_CURLY a);
      (")", fun a -> RIGHT_PAREN a);
      ("->", fun a -> ARROW a);
      ("|}", fun a -> BAR_RIGHT_CURLY a);
      ("|]", fun a -> BAR_RIGHT_SQUARE a);
      ("|>", fun a -> BAR_GT a);

      (":=", fun a -> COLON_EQ a);
      ("=>", fun a -> DARROW a);
      ("==>", fun a -> DDARROW a);
    ]

let comment_depth = ref 0

let string_of = Lexing.lexeme
let annot_of Lexing.{lex_curr_p; _} = lex_curr_p

let create_id lexbuf =
  let annot = annot_of lexbuf in
  let str = string_of lexbuf in
  try
    (Hashtbl.find keyword_tbl str) annot
  with _ -> (
      match String.get str 0 with
      | c when 'A' <= c && c <= 'Z' -> CAPITALIZED_ID {annot; value= str}
      | _ -> LOWERCASE_ID {annot; value= str}
    )

let string_buffer = ref (Bytes.create 2048)
let string_end = ref 0

let reset_string () = string_end := 0

let add_string ch =
  let x = !string_end in
  let buffer = !string_buffer in
  if x = Bytes.length buffer then (
    let new_buffer = Bytes.create (x * 2) in
    Bytes.blit buffer 0 new_buffer 0 x ;
    Bytes.set new_buffer x ch ;
    string_buffer := new_buffer ;
    string_end := x + 1
  )
  else (
    Bytes.set buffer x ch ;
    string_end := x + 1
  )

let get_string () = Bytes.sub_string !string_buffer 0 !string_end

let ready filename chan =
  (* Don't know why, but even ~with_positions:true does not give me a
     proper file name in pos_fname ... *)
  let lexbuf = Lexing.from_channel chan in
  Lexing.set_filename lexbuf filename ;
  lexbuf
}

let blank = [' ' '\009' '\012' '\t']+
let integer = ['0'-'9']+
let real = ['0'-'9']+ '.' ['0'-'9']+
let identifier = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*


rule main = parse
  | blank { main lexbuf }
  | blank*"\n" { main lexbuf }
  | "(*"
      { incr comment_depth;
        comment lexbuf;
        main lexbuf
      }
  | integer
      {
        Parser.INT {annot= annot_of lexbuf; value= int_of_string (string_of lexbuf)}
      }
  | real
      {
        Parser.FLOAT {annot= annot_of lexbuf; value= float_of_string (string_of lexbuf)}
      }
  | identifier
      {
        create_id lexbuf
      }
  | ":=" | "<:" | "<-" | "->" | "=>" | "==>"
  | "{|" | "|}" | "<|" | "|>" | "[|" | "|]" | "=="
    {
      create_id lexbuf
    }
  | ['~' '%' '\\' '+' '-' '&' '|' ':' '@' '`' '$']+
    {
      create_id lexbuf
    }
  | ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ',' '=' '\'']
    {
      create_id lexbuf
    }
  | "\""
    {
      reset_string () ;
      string lexbuf
    }
  | eof { Parser.EOF (annot_of lexbuf) }
  | _
    {
      Error.error_at (annot_of lexbuf) (Format.sprintf "Illegal character: %s" (string_of lexbuf))
    }
and comment = parse
  | "*)"
      {
        decr comment_depth;
        if !comment_depth > 0 then comment lexbuf
      }
  | "(*"
      {
        incr comment_depth;
        comment lexbuf
      }
  | eof
      {
        Error.error_at  (annot_of lexbuf) "Comment not terminated"
      }
  | '\n' { comment lexbuf }
  | "\r\n" { comment lexbuf }
  | _ { comment lexbuf }
and string = parse
  | '"'
      {
        Parser.STRING {annot= (annot_of lexbuf); value= get_string ()}
      }
  | '\\'
      {
        add_string (escaped lexbuf) ;
        string lexbuf
      }
  | '\n'
      {
        add_string '\n' ;
        string lexbuf
      }
  | eof
      {
        Error.error_at (annot_of lexbuf) "String not terminated"
      }
  | _
      {
        add_string (Lexing.lexeme_char lexbuf 0) ;
        string lexbuf
      }
and escaped = parse
  | 'n' { '\n' }
  | 't' { '\t' }
  | '\\' { '\\' }
  | '"' { '\034' }
  | '\'' { '\'' }
  | ['0'-'9']['0'-'9']['0'-'9']
      {
        let ev = int_of_string (string_of lexbuf) in
        if ev > 255 then
          Error.error_at (annot_of lexbuf) (Format.sprintf "Invalid escaped value %i" ev)
        else Char.chr ev
      }
  | [^ '"' '\\' 't' 'n' '\'']
      {
        Error.error_at (annot_of lexbuf) "Invalid escaped value"
      }
