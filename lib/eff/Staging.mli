type mode =
  | Inner
  | Outer

val run : stage:int -> (unit -> 'a) -> 'a

val get_stage : unit -> int
val incr_stage : int -> (mode -> 'a) -> 'a
val decr_stage : int -> (mode -> 'a) -> 'a
