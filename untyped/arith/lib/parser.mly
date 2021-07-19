%{
open Util
open Syntax
%}

/* Keyword tokens */
%token <Util.annot> IMPORT
%token <Util.annot> IF
%token <Util.annot> THEN
%token <Util.annot> ELSE
%token <Util.annot> TRUE
%token <Util.annot> FALSE
%token <Util.annot> SUCC
%token <Util.annot> PRED
%token <Util.annot> ISZERO

/* Identifier, constant */
%token <string Util.with_annot> CAPITALIZED_ID
%token <string Util.with_annot> LOWERCASE_ID
%token <int Util.with_annot> INT
%token <float Util.with_annot> FLOAT
%token <string Util.with_annot> STRING

/* Symbolic tokens */
%token <Util.annot> APOSTROPHE
%token <Util.annot> DOUBLE_QUOTE
%token <Util.annot> ARROW
%token <Util.annot> BANG
%token <Util.annot> BAR_GT
%token <Util.annot> BAR_RIGHT_CURLY
%token <Util.annot> BAR_RIGHT_SQUARE
%token <Util.annot> COLON
%token <Util.annot> DOUBLE_COLON
%token <Util.annot> COLON_EQ
%token <Util.annot> COMMA
%token <Util.annot> DARROW
%token <Util.annot> DDARROW
%token <Util.annot> DOT
%token <Util.annot> EOF
%token <Util.annot> EQ
%token <Util.annot> DOUBLE_EQ
%token <Util.annot> GT
%token <Util.annot> HASH
%token <Util.annot> LEFT_CURLY
%token <Util.annot> LEFT_CURLY_BAR
%token <Util.annot> LEFT_ARROW
%token <Util.annot> LEFT_PAREN
%token <Util.annot> LEFT_SQUARE
%token <Util.annot> LEFT_SQUARE_BAR
%token <Util.annot> LT
%token <Util.annot> RIGHT_CURLY
%token <Util.annot> RIGHT_PAREN
%token <Util.annot> RIGHT_SQUARE
%token <Util.annot> SEMICOLON
%token <Util.annot> SLASH
%token <Util.annot> STAR
%token <Util.annot> TRIANGLE
%token <Util.annot> UNDER_SCORE
%token <Util.annot> BAR


%start toplevel
%type <Syntax.command list> toplevel
%%

/* Main parser definition */

/* The toplevel of a file is a sequence of commands, each terminated by a semicolon */
toplevel:
  | EOF
    {
      []
    }
  | Command SEMICOLON toplevel
    {
      let cmd = $1 in
      let cmds = $3 in
      cmd :: cmds
    }

Command:
  | Term
    {
      let t = $1 in
      Eval(annot_of t, t)
    }

Term:
  | App { $1 }
  | IF Term THEN Term ELSE Term
    {
      If ($1, $2, $4, $6)
    }


App:
  | Atomic { $1 }
  | SUCC Atomic { Succ ($1, $2) }
  | PRED Atomic { Pred ($1, $2) }
  | ISZERO Atomic { IsZero ($1, $2) }

Atomic:
  | LEFT_PAREN Term RIGHT_PAREN { $2 }
  | TRUE { True ($1) }
  | FALSE { False ($1) }
  | INT
    {
      let rec f n = match n with
	| 0 -> Zero ($1.annot)
	| n -> Succ ($1.annot, f (n-1))
      in
      f $1.value
    }
