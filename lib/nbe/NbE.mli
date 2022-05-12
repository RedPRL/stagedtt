open Core
open TermBuilder

module S := Syntax
module D := Domain

(** {1 Evaluation} *)
val eval : stage:int -> env:D.env -> S.t -> D.t
val eval_tp : stage:int -> env:D.env -> S.tp -> D.tp

val do_el : D.t -> D.tp

(** {1 Quoting} *)
val quote : size:int -> D.t -> S.t
val quote_tp : size:int -> D.tp -> S.tp

(** {1 Conversion Checking} *)
val equate : size:int -> D.t -> D.t -> unit
val equate_tp : size:int -> D.tp -> D.tp -> unit

exception NotConvertible

(** {1 Grafting} *)
val graft_value : S.t Graft.t -> D.t
val graft_tp : S.tp Graft.t -> D.tp

(** {1 Closure Instantiation} *)
val inst_tm_clo : D.tm_clo -> D.t -> D.t
val inst_tp_clo : D.tp_clo -> D.t -> D.tp

