open Command

type status =
  | Continue
  | Quit

val exec_command : command -> status
val exec : command list -> unit
val load : [ `Stdin | `File of string ] -> unit
