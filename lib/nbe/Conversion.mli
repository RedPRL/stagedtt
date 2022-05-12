open Core

module D := Domain

exception NotConvertible

val equate : size:int -> D.t -> D.t -> unit
val equate_tp : size:int -> D.tp -> D.tp -> unit
