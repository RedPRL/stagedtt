open Core
open TermBuilder

module S = Syntax
module D = Domain

(** {1 Evaluation} *)
val unfold : D.t -> D.t
val eval : env:D.env -> S.t -> D.t
val eval_tp : env:D.env -> S.tp -> D.tp

(** {1 Eliminators} *)
val do_ap : D.t -> D.t -> D.t
val do_splice : D.t -> D.t
val do_el : D.t -> D.tp

val unfold_el : D.code -> D.tp

(** {1 Closure Instantiation} *)
val inst_tm_clo : D.tm_clo -> D.t -> D.t
val inst_tp_clo : D.tp_clo -> D.t -> D.tp

(** {1 Grafting} *)
val graft_value : S.t Graft.t -> D.t
val graft_tp : S.tp Graft.t -> D.tp
