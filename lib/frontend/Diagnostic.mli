type severity =
  | Info
  | Warning
  | Error

type note


type cause
type t

val cause : filename:string -> row:int -> column:int -> cause
val note : source_code:string -> ?row:int -> ?start_col:int -> ?end_col:int -> note:string -> cause -> cause
val help : string -> cause -> cause

val info : ?cause:cause -> code:string -> string -> t 
val warning : ?cause:cause -> code:string -> string -> t 
val error : ?cause:cause -> code:string -> string -> t 

val pp : Format.formatter -> t -> unit
