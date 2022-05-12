open Core

module S := Syntax
module O := Outer
module I := Inner

val stage : stage:int -> S.t -> S.t

val eval_inner : stage:int -> S.t -> I.t
val eval_outer : stage:int -> S.t -> int -> O.t

val quote_inner : size:int -> I.t -> S.t


module Effectful : sig
  (** Requires {!val:Eff.Staging.run} *)
  val eval_inner : S.t -> I.t

  (** Requires {!val:Eff.Staging.run} *)
  val eval_outer : S.t -> int -> O.t
end
