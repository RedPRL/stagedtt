open Prelude

type _ Effect.t +=
  | Survivable : Diagnostic.t -> unit Effect.t
  | Fatal : Diagnostic.t -> 'a Effect.t

type env =
  { lexbuf : Lexing.lexbuf;
    span   : Span.t option
  }

module Reader = Algaeff.Reader.Make (struct type nonrec env = env end)

let run lexbuf k =
  Reader.run ~env:{ lexbuf; span = None } k

let locate span k =
  Reader.scope (fun env -> { env with span = span }) k

let with_cause k =
  let env = Reader.read () in
  let cause =
    env.span
    |> Option.map @@ fun span ->
    Diagnostic.cause
      ~filename:(Span.filename span)
      ~row:(Span.start_row span)
      ~column:(Span.start_column span)
  in k cause

let with_note ?note cause =
  let env = Reader.read () in
  match note, cause, env.span with
  | Some note, Some cause, Some span ->
    let size = (span.stop.pos_cnum - span.start.pos_bol) in
    let buff = Bytes.create size in
    Bytes.blit env.lexbuf.lex_buffer span.start.pos_bol buff 0 size; 
    let source_code = Bytes.to_string buff in
    Option.some @@
    Diagnostic.with_note
      ~source_code
      ~row:(Span.start_row span)
      ~start_col:(Span.start_column span)
      ~end_col:(Span.stop_column span)
      ~msg:note
      cause
  | _ -> None


let info ?note ~code msg =
  let diag =
    with_cause @@ fun cause ->
    Diagnostic.error ?cause:(with_note ?note cause) ~code msg
  in Effect.perform (Survivable diag)

let warning ?note ~code msg =
  let diag =
    with_cause @@ fun cause ->
    Diagnostic.error ?cause:(with_note ?note cause) ~code msg
  in Effect.perform (Survivable diag)

let error ?note ~code msg =
  let diag =
    with_cause @@ fun cause ->
    Diagnostic.error ?cause:(with_note ?note cause) ~code msg
  in Effect.perform (Fatal diag)

let impossible ?note msg =
  let callstack =
    Format.asprintf "Callstack:@.%s@." @@ 
    Printexc.raw_backtrace_to_string @@
    Printexc.get_callstack 1000
  in
  let diag =
    with_cause @@ fun cause ->
    let cause =
      cause
      |> with_note ?note
      |> Option.map (Diagnostic.help callstack)
    in 
    Diagnostic.impossible ?cause:(with_note ?note cause) ~code:"XXXXX" msg
  in Effect.perform (Fatal diag)
