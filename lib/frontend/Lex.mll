{
open Prelude
open Span
open Lexing
open Grammar

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }

let make_table num elems =
  let table = Hashtbl.create num in
  List.iter (fun (k, v) -> Hashtbl.add table k v) elems;
  table

let commands =
  make_table 0 [
    ("#fail", FAIL);
    ("#normalize", NORMALIZE);
    ("#stage", STAGE);
    ("#print", PRINT);
    ("#debug", DEBUG);
    ("#quit", QUIT);
  ]

let keywords =
  make_table 0 [
    ("def", DEF);
    ("def!", DEF_BANG);
    ("type", TYPE);
    ("the", THE)
  ]

(* Some Lexing Utilities *)
let last_token lexbuf = 
  let tok = lexeme lexbuf in
  if tok = "" then None else Some tok

let current_span lexbuf = 
  {start = lexbuf.lex_start_p; stop = lexbuf.lex_curr_p}

}

let line_ending
  = '\r'
  | '\n'
  | "\r\n"
let whitespace =
  [' ' '\t']+

let atom_initial =
  [^ '0'-'9' '-'     '?' '!' '(' ')' '[' ']' '{' '}' '<' '>' '.' '#' '\\' '@' '*' '^' ':' ',' ';' '|' '=' '"' '`' ' ' '\t' '\n' '\r']

let atom_subsequent =
  [^                         '(' ')' '[' ']' '{' '}' '<' '>' '.' '#' '\\' '@' '*' '^' ':' ',' ';' '|' '=' '"'     ' ' '\t' '\n' '\r']

let atom = atom_initial atom_subsequent*

let hole = '?' atom_subsequent*


let number =
  ['0'-'9']+

let type =
  "type" number

(* Whitespace/comments *)
rule line_comment kont = parse
  | line_ending
    { new_line lexbuf; kont lexbuf }
  | _
    { line_comment kont lexbuf }

and skip_whitespace kont = parse
  | "--"
    { line_comment (skip_whitespace kont) lexbuf }
  | line_ending
    { new_line lexbuf; (skip_whitespace kont) lexbuf }
  | whitespace
    { skip_whitespace kont lexbuf }
  | ""
    { kont lexbuf }

and token = parse "" { skip_whitespace real_token lexbuf }

and real_token = parse
  (* Symbols *)
  | "\\" | "λ"
    { LAMBDA }
  | "->" | "→"
    { RIGHT_ARROW }
  | ':'
     { COLON }
  | "::"
    { COLON_COLON }
  | '_'
    { UNDERSCORE }
  | ":="
    { COLON_EQUALS }
  (* Delimiters *)
  | '('
    { LPR }
  | ')'
    { RPR }
  | '['
    { LSQ }
  | ']'
    { RSQ }
  | "↑["
    { UP_LSQ }
  | "⇑["
    { DOUBLE_UP_LSQ }
  | "↓["
    { DOWN_LSQ }
  | number
    { NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | "on"
    { FLAG true }
  | "off"
    { FLAG false }
  | "#" atom_subsequent+
    {
      let input = lexeme lexbuf in
      match Hashtbl.find commands input with
      | tok -> tok
      | exception Not_found -> Printf.eprintf "Unknown Command: %s\n" (lexeme lexbuf); token lexbuf
    }
  | "?"
    { HOLE None }
  | hole
    {
      let str = lexeme lexbuf in
      let len = String.length str in
      if len = 1 then
        HOLE None
      else
        let hole_name = String.sub str 1 (String.length str - 1) in
        HOLE (Some hole_name)
    }
  | atom
    {
      (* [TODO: Reed M, 02/05/2022] Actually disallow subscripts *)
      (* See [NOTE: Pretty Printing + Renaming Variables] to see why we disallow numeric unicode subscripts. *)
      let input = lexeme lexbuf in
      match Hashtbl.find keywords input with
      | tok -> tok
      | exception Not_found -> Grammar.ATOM input
    }
  | eof
    { EOF }
  | _
    { raise @@ SyntaxError (lexeme lexbuf) }
