open Algaeff
open Prelude
open Bwd

module S := Core.Syntax
module D := Core.Domain

module CS := Syntax

(** {1 Effects} *)

type _ Effect.t +=
  | Resolve : Yuujinchou.Trie.path -> (D.t Lazy.t * D.tp) Effect.t
  (** We invoke a {!constr:Resolve} effect every time we need to resolve some non-local identifier. *)

val check_tp : CS.t -> stage:int -> S.tp
val infer_tp : CS.t -> S.tp * int

val check : CS.t -> D.tp -> S.t
val infer : CS.t -> S.t * D.tp
