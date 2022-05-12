open Core

module S := Syntax
module I := Inner

val quote_inner : size:int -> I.t -> S.t
