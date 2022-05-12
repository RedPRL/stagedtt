open Core

module D := Domain

val unfold : stage:int -> size:int -> D.t -> D.t
val unfold_top : stage:int -> D.t -> D.t
