open Bwd

module S := Core.Syntax
module D := Core.Domain

module CS := Syntax

val check_tp : CS.t -> stage:int -> S.tp
val infer_tp : CS.t -> S.tp * int

val check : CS.t -> D.tp -> S.t
val infer : CS.t -> S.t * D.tp
