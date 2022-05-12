open Core

module S := Syntax
module O := Outer
module I := Inner

val stage : tm_stage:int -> S.t -> S.t

val eval_inner : tm_stage:int -> S.t -> I.t
val eval_outer : tm_stage:int -> S.t -> int -> O.t

val quote_inner : size:int -> I.t -> S.t
