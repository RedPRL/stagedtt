open Prelude
module D := Data

type env = D.value_env

type 'a clo = 'a D.vclo = 
  | Clo of 'a * env

type tm_clo = D.syntax clo
type tp_clo = D.syntax_tp clo


type t = D.value =
  | Lam of Ident.t * tm_clo
  | Quote of D.value
  | Neu of D.neu
  | Code of code

and tp = D.value_tp =
  | Pi of tp * Ident.t * tp_clo
  | Expr of tp
  | El of code
  | ElNeu of neu
  | Univ of int

and code = D.code = 
  | CodePi of t * t
  | CodeUniv of int

and neu = D.neu = { hd : hd; spine : frm list } 

and hd = D.hd = 
  | Local of int
  | Global of global
  | Hole of string option

and global =
  [ `Unstaged of Ident.path * D.value Lazy.t * D.inner Lazy.t ]

and frm = D.frm =
  | Ap of t
  | Splice


val local : int -> t
val global : Ident.path -> t Lazy.t -> D.inner Lazy.t -> t 
val hole : string option -> t
val push_frm : neu -> frm -> unfold:(t -> t) -> stage:(D.inner -> D.inner)-> neu

(** {1 Pretty Printing} *)
val pp : t Pp.printer
val pp_tp : tp Pp.printer

module Env : sig
  val empty : env

  (** Create an environment from a list of values
      NOTE: This also takes in the length of the list
      to avoid performing an O(n) computation. *)
  val from_vals : t Lazy.t bwd -> int -> env

  (** Lookup a DeBrujin Index in an environment. *)
  val lookup_idx : env -> int -> t Lazy.t option

  (** Lookup a DeBrujin Index in an environment. *)
  val lookup_tp_idx : env -> int -> tp option

  (** Get all the local bindings in an environment. *)
  val locals : env -> t Lazy.t bwd

  (** Get the number of values in the environment. *)
  val size : env -> int

  (** Get the number of types in the environment. *)
  val tp_size : env -> int

  (** Extend an environment with a value. *)
  val extend : env -> t -> env

  (** Extend an environment with a type. *)
  val extend_tp : env -> tp -> env
end
