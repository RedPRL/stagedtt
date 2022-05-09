open Containers

(* Ordered Hash Tables ala Hettinger.

   See https://github.com/python/cpython/blob/329afe78c3bbc234492a53f7a4084d07e215a077/Objects/dictobject.c
   for implementation notes.

   Note that we only need to support 'pop', not arbitrary deletes, as the intended use is for contexts.
   This simplifies the implementation quite a bit.
*)

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

(** Ordered Hash Tables ala Hettinger. *)
module Make(H : Hashtbl.HashedType) : (S with type key = H.t) =
struct
  type key = H.t

  type 'a entry = {
    hash : int;
    key : key;
    value : 'a
  }

  (* [HACK: Uninitialized Entries] We should probably handle this inside of resize... *)
  type null = { null : int }
  let null : 'a entry =
    { hash = -1;
      key = Obj.magic { null = 0 };
      value = Obj.magic { null = 0 } }

  type 'a t = {
    mutable num_entries : int;
    (** The number of entries in the table. *)
    mutable entries : 'a entry array;
    (** The actual entries of the hash table *)
    mutable indices : int array;
    (** The mapping of hashes to their location in the {!recfield:entries} array. *)
    mutable index_size : int;
    (** The size of the {!recfield:hash_indicies} array.
        [INVARIANT]: Must be a power of two. *)
  }

  let free_slot = -1

  (** Find the next power of 2. f*)
  let next_power_of_2 n =
    let n = ref n in
    let r = ref 1 in
    while !n <> 0  do
      n := Int.shift_right_logical !n 1;
      r := Int.shift_left !r 1;
    done;
    !r

  let create size =
    let n = next_power_of_2 size in
    { num_entries = 0;
      (* HACK: This is somewhat evil, but we need to fill the array
         with uninitialized data. *)
      entries = Array.make n null;
      index_size = size;
      indices = Array.make n free_slot;
    }

  type index =
    | Free of int
    | Found of { entry_index : int; hash_index : int }

  (** Try to find an open slot in the {!recfield:hash_indicies} array
      for a given hash.

      This uses a trick from Tim Peters where we use a linear congruent
      PRNG to probe in different patterns based off the hash. *)
  let hash_index k_hash tbl =
    let mask = tbl.index_size - 1 in
    let i = ref @@ (Int.abs k_hash) land mask in
    let perturb = ref k_hash in
    let exception Break of index in
    try
      while true do
        let hash_index = !i land mask in
        let entry_index = tbl.indices.(hash_index) in
        if (entry_index = free_slot) then
          raise @@ Break (Free hash_index)
        else if tbl.entries.(entry_index).hash = k_hash then
          raise @@ Break (Found { hash_index; entry_index })
        else (
          i := (5 * !i + !perturb + 1);
          perturb := !perturb lsr 5
        )
      done;
      failwith "hash_index: the impossible happened."
    with Break res -> res

  let[@inline] should_resize tbl =
    (* As the table fills up, lookups will slow down.
       Therefore, we resize once the table is 2/3 full *)
    3 * tbl.num_entries > 2 * tbl.index_size

  let[@inline] resize tbl =
    let new_size = next_power_of_2 (3 * tbl.num_entries) in
    let new_entries = Array.make new_size null in
    let new_indices = Array.make new_size free_slot in
    Array.blit tbl.entries 0 new_entries 0 tbl.num_entries;
    tbl.entries <- new_entries;
    tbl.index_size <- new_size;
    tbl.indices <- new_indices;
    (* Recompute all the hash indicies. *)
    for i = 0 to tbl.num_entries do
      match hash_index tbl.entries.(i).hash tbl with
      | Free ix -> 
        tbl.indices.(ix) <- i
      | Found _ ->
        failwith "resize: the impossible happened@."
    done

  let add key value tbl =
    let hash = H.hash key in
    match hash_index hash tbl with
    | Found { entry_index; _} ->
      tbl.entries.(entry_index) <- { hash; key; value }
    | Free hash_index ->
      tbl.indices.(hash_index) <- tbl.num_entries;
      tbl.entries.(tbl.num_entries) <- { hash; key; value };
      tbl.num_entries <- tbl.num_entries + 1;
      if should_resize tbl then
        resize tbl
      else
        ()

  let get key tbl =
    let hash = H.hash key in
    match hash_index hash tbl with
    | Found { entry_index;_} ->
      Some (tbl.entries.(entry_index).value)
    | Free _ -> None

  let get_idx key tbl =
    let hash = H.hash key in
    match hash_index hash tbl with
    | Found { entry_index; _} ->
      Some entry_index
    | _ -> None

  (** Remove the last entry added to the hashtable. *)
  let pop tbl =
    let last_entry = tbl.num_entries - 1 in
    let entry = tbl.entries.(last_entry) in
    match hash_index entry.hash tbl with
    | Found {hash_index; _} ->
      (* Make sure we don't keep a reference to the entry around. *)
      tbl.entries.(last_entry) <- null;
      tbl.num_entries <- last_entry;
      tbl.indices.(hash_index) <- free_slot;
      (entry.key, entry.value)
    | _ -> failwith "pop: the impossible happened!"
end
