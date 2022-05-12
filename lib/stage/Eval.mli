open Core

module S := Syntax
module I := Inner

val eval_inner : stage:int -> tm_stage:int -> S.t -> I.t
