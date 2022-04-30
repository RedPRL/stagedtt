module S := Syntax
module D := Domain

type 'a tb

(** {1 Term Builders} *)
module TB : sig
  (** {2 Pi} *)
  val pi : ?ident:string -> S.tp tb -> (S.t tb -> S.tp tb) -> S.tp tb
  val lam : ?ident:string -> (S.t tb -> S.t tb) -> S.t tb
  val ap : S.t tb -> S.t tb -> S.t tb

  (** {2 Signatures} *)
  val sign : (string * S.tp tb) list -> S.tp tb

  (** {2 Universes} *)
  val el : S.t tb -> S.tp tb
end

(** {1 Grafting} *)
(* [TODO: Reed M, 29/04/2022] Explain this better...*)
module Graft : sig
  (** An abstract type of "grafted" values. *)
  type 'a t

  (** Graft a value. *)
  val value : D.t -> (S.t tb -> 'a t) -> 'a t

  (** Graft a type. *)
  val tp : D.tp -> (S.tp tb -> 'a t) -> 'a t

  val fields : (string * D.t) list -> ((string * S.t tb) list -> 'a t) -> 'a t

  val build : 'a tb -> 'a t

  val graft : 'a t -> 'a * D.env
end
