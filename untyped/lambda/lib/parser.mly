%{
open Util
open Syntax
%}

/* Keyword tokens */
%token <Util.annot> LAMBDA

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
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel
%%

/* Main parser definition */

/* The toplevel of a file is a sequence of commands, each terminated by a semicolon */
toplevel:
  | EOF
    {
      fun ctx -> [], ctx
    }
  | Command SEMICOLON toplevel
    {
      fun ctx ->
      let cmd, ctx = $1 ctx in
      let cmds, ctx = $3 ctx in
      cmd :: cmds, ctx
    }

Command:
  | Term
    {
      fun ctx -> (let t = $1 ctx in Eval(annot_of t, t)), ctx
    }
  | LOWERCASE_ID Binder
    {
      fun ctx -> ((Bind ($1.annot, $1.value, $2 ctx)), add_name ctx $1.value)
    }

Binder:
  | SLASH
    {
      fun _ctx -> NameBind
    }

Term:
  | AppTerm
    {
      $1
    }
  | LAMBDA LOWERCASE_ID DOT Term
    {
      fun ctx ->
      let ctx' = add_name ctx $2.value in
      Abstraction{annot= $1; binder= $2.value; body= $4 ctx'}
    }
  | LAMBDA UNDER_SCORE DOT Term
    {
      fun ctx ->
      let ctx' = add_name ctx "_" in
      Abstraction{annot= $1; binder= "_"; body= $4 ctx'}
    }

AppTerm:
  | ATerm
    {
      $1
    }
  | AppTerm ATerm
    {
      fun ctx ->
      let e1 = $1 ctx in
      let e2 = $2 ctx in
      Application{annot= annot_of e1; lhs= e1; rhs= e2}
    }

/* Atomic terms are ones that never require extra parentheses */
ATerm:
  | LEFT_PAREN Term RIGHT_PAREN
    {
      $2
    }
  | LOWERCASE_ID
    {
      fun ctx ->
      Variable{annot= $1.annot;
	       index= index_of_name $1.annot ctx $1.value;
	       len_ctx= length_of ctx}
    }
