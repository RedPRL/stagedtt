open Prelude

type _ Effect.t +=
  | Survivable : Diagnostic.t -> unit Effect.t
  | Fatal : Diagnostic.t -> 'a Effect.t

val run : Lexing.lexbuf -> (unit -> 'a) -> 'a
val locate : Span.t option -> (unit -> 'a) -> 'a

val info : ?note:string -> code:string -> string -> unit
val warning : ?note:string -> code:string -> string -> unit
val error : ?note:string -> code:string -> string -> 'a
val impossible : ?note:string -> string -> 'a
