module S := Core.Syntax
module D := Core.Domain

module CS := Syntax

type env

val check_tp : env:env -> CS.t -> stage:int -> S.tp
val infer_tp : env:env -> CS.t -> S.tp * int
