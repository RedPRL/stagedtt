val dump_callstack : ?depth:int -> string -> unit
val debug_mode : bool -> unit
val is_debug_mode : unit -> bool
val print : ('a, Format.formatter, unit) format -> 'a
