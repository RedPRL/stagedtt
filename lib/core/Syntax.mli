open Prelude
module D := Data

type t = D.syn =
  | Local of int
  | Global of Ident.path * D.value Lazy.t
  | Staged of Ident.path * D.outer Lazy.t * D.value Lazy.t

  | Lam of Ident.t * t
  | Ap of t * t

  | Quote of t
  | Splice of t

  | CodePi of t * t
  | CodeUniv of int

type tp = D.syn_tp =
  | TpVar of int
  (** DeBruijin-Indexed type variables.
      These are used during grafting. *)
  | Pi of tp * Ident.t * tp
  | Expr of tp
  | El of t
  | Univ of int

(* include module type of Syntax *)

val app : t -> t -> t
val apps : t -> t list -> t

(** {1 Pretty Printing} *)
val pp : t Pp.printer
val pp_tp : tp Pp.printer

(** {1 Debug Printing} *)
val dump : Format.formatter -> t -> unit
