open Prelude
open Data

include module type of Domain

val local : int -> Domain.t
val global : Ident.t -> Domain.t Lazy.t -> Domain.t 
val push_frm : Domain.neu -> Domain.frm -> unfold:(Domain.t -> Domain.t) -> Domain.neu

(** {1 Pretty Printing} *)
val pp : t Pp.printer
