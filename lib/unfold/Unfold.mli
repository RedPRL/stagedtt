open Core

module D := Domain
module I := Inner

val unfold : stage:int -> size:int -> D.t -> D.t
val unfold_top : stage:int -> D.t -> D.t
val unfold_inner : I.t -> I.t
