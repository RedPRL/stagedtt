module type S =
sig
  type key
  type 'a t

  val create : int -> 'a t

  val add : key -> 'a -> 'a t -> unit
  val get : key -> 'a t -> 'a option
  val get_idx : key -> 'a t -> int option
  val pop : 'a t -> key * 'a
end

module Make : functor (H : Hashtbl.HashedType) -> (S with type key = H.t)
