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
  (* [TODO: Reed M, 02/05/2022] Handle errors better here. *)
  let cmds = Grammar.commands Lex.token lexbuf in
  close_in chan;
  cmds
