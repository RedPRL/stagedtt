module type S =
sig
  type key
  type 'a t

  (** Create a new, empty hashtable with a specified initial capacity.*)
  val create : int -> 'a t

  (** Add a new binding to the hash table, shadowing the previous bindings. *)
  val push : key -> 'a -> 'a t -> unit

  (** Remove the last binding from the table, potentially unshadowing. *)
  val pop : 'a t -> key * 'a

  (** Get the most recently bound value for a given key. *)
  val get : key -> 'a t -> 'a option

  (** Get the DeBruijin index of the most recently bound value for a given key. *)
  val get_idx : key -> 'a t -> int option

end

module Make : functor (H : Hashtbl.HashedType) -> (S with type key = H.t)
