module S = Syntax
module D = Domain

val push_frm : D.neu -> D.frm -> unfold:(D.t -> D.t) -> D.neu

val eval : env:D.env -> S.t -> D.t
val eval_tp : env:D.env -> S.tp -> D.tp

val inst_tm_clo : D.tm_clo -> D.t -> D.t
val inst_tp_clo : D.tp_clo -> D.t -> D.tp
val inst_sign_clo : D.sign_clo -> D.t -> D.sign

val do_ap : D.t -> D.t -> D.t
val do_proj : D.t -> string -> D.t
val do_splice : D.t -> D.t
val do_el : D.t -> D.tp
