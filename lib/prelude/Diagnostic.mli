type severity =
  | Info
  | Warning
  | Error
  | Impossible

type note
type cause
type t

val cause : filename:string -> row:int -> column:int -> cause
val note : source_code:string -> row:int -> ?start_col:int -> ?end_col:int -> string -> note

val with_note : source_code:string -> ?row:int -> ?start_col:int -> ?end_col:int -> msg:string -> cause -> cause
val help : string -> cause -> cause
val add_note : note -> cause -> cause

val info : ?cause:cause -> code:string -> string -> t 
val warning : ?cause:cause -> code:string -> string -> t 
val error : ?cause:cause -> code:string -> string -> t 
val impossible : ?cause:cause -> code:string -> string -> t 

val severity : t -> severity

val pp : Format.formatter -> t -> unit
