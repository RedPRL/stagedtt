open Core

module S := Syntax
module O := Outer
module I := Inner

val eval_inner : S.t -> I.t
val eval_outer : S.t -> int -> O.t
