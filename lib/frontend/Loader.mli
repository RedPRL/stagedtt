open Prelude
open Command

val load : [`Stdin | `File of string] -> (command list * Lexing.lexbuf, Diagnostic.t) result 
