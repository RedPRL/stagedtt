open Prelude

let error_cause (span : Span.t) ~note =
  let filename = span.start.pos_fname in
  let row = span.start.pos_lnum in
  let column = span.start.pos_cnum - span.start.pos_bol in
  let end_col = span.stop.pos_cnum - span.start.pos_bol in
  let source_code =
    (* HACK: When displaying a lexer/parser error, we will need to get the relevant piece
       of source code. However, if we read from stdin, this isn't very easy... *)
    Option.value ~default:"" @@
    match span.start.pos_fname with
    | "[stdin]" -> None
    | fname ->
      In_channel.with_open_bin fname @@ fun chan ->
      In_channel.seek chan (Int64.of_int span.start.pos_bol);
      In_channel.input_line chan
  in
  Diagnostic.cause ~filename ~row ~column
  |> Diagnostic.with_note ~source_code ~row ~start_col:column ~end_col ~msg:note

let lex_error token span =
  Diagnostic.error
    ~cause:(error_cause span ~note:(Format.asprintf "The token '%s' is unrecognized." token))
    ~code:"L0001"
    "Unrecognized token"

let parse_error span =
  Diagnostic.error
    ~cause:(error_cause span ~note:(Format.asprintf "There was a parse error right here."))
    ~code:"P0001"
    "Parse error"

let try_parse parser lexbuf =
  try Ok (parser Lex.token lexbuf) with
  | Lex.SyntaxError token ->
    let span = Lex.current_span lexbuf in
    Error (lex_error token span)
  | Grammar.Error ->
    let span = Lex.current_span lexbuf in
    Error (parse_error span)

let create_lexbuf input =
  let chan, fname =
    match input with
    | `Stdin -> stdin, "[stdin]"
    | `File fname -> open_in fname, fname
  in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  chan, lexbuf

let load input =
  let chan, lexbuf = create_lexbuf input in
  let cmds = try_parse Grammar.commands lexbuf in
  close_in chan;
  cmds
  |> Result.map @@ fun cmds -> (cmds, lexbuf)
