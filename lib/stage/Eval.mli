open Core

module S := Syntax
module O := Outer
module I := Inner

val eval_inner : tm_stage:int -> S.t -> I.t
val eval_outer : tm_stage:int -> S.t -> int -> O.t
