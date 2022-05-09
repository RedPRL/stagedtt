open Prelude

type env
type 'a printer = env -> Format.formatter -> 'a -> unit

val init : env

(** {1 Precedences} *)
module Prec : sig
  type t
  val nonassoc : int -> t
  val left : int -> t
  val right : int -> t
  val prefix : int -> t
  val postfix : int -> t
end

val left_of : Prec.t -> env -> env
val right_of : Prec.t -> env -> env
val surrounded_by : Prec.t -> env -> env
val isolated : env -> env
val isolate_left : Prec.t -> env -> env
val isolate_right : Prec.t -> env -> env

val bind_var : Ident.t -> env -> string * env
val var : int printer
val lvl : int printer

val parens : ('a -> Prec.t) -> env -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
