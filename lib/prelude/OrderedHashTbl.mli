open Bwd

module type S =
sig
  type key
  type 'a t

  (** Create a new, empty hashtable with a specified initial capacity.*)
  val create : int -> 'a t

  (** Return the number of entries in the hash table. *)
  val size : 'a t -> int

  (** Add a new binding to the hash table, shadowing the previous bindings. *)
  val push : key -> 'a -> 'a t -> unit

  (** Remove the last binding from the table, potentially unshadowing. *)
  val pop : 'a t -> key * 'a

  (** Bind a value to a key for the duration of a function. *)
  val scope : key -> 'a -> 'a t -> (unit -> 'r) -> 'r

  (** Get the last binding in the table, but don't remove it. *)
  val peek : 'a t -> key * 'a

  (** Get the most recently bound value for a given key. *)
  val find : key -> 'a t -> 'a option

  (** Get the DeBruijin index of the most recently bound value for a given key. *)
  val find_idx_of : key -> 'a t -> int option

  (** Get the nth last thing bound. *)
  val nth : int -> 'a t -> key * 'a

  (** Get all the values bound inside of the map. *)
  val values : 'a t -> 'a bwd

  (** Get all the values bound inside of the map by using a projection function *)
  val values_with : 'a t -> ('a -> 'b) -> 'b bwd
end

module Make : functor (H : Hashtbl.HashedType) -> (S with type key = H.t)
