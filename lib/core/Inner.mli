open Prelude
module D := Data


(** The inner syntax is basically the same as the outer syntax, but with DeBruijin levels instead
    of indicies. *)
type t = D.inner =
  | Local of int
  | Global of global

  | Lam of Ident.t * t
  | Ap of t * t

  | Quote of t
  | Splice of t

  | CodePi of t * t
  | CodeUniv of int

and global =
  [ `Unstaged of Ident.path * D.value Lazy.t * D.inner Lazy.t
  | `Staged of Ident.path * D.value Lazy.t * D.inner Lazy.t * (int -> D.outer)
  ]
