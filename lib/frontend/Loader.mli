open Prelude
open Command

val load : [`Stdin | `File of string] -> (command list, Diagnostic.t) result
