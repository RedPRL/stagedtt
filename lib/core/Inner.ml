open Prelude
module D = Data

type t = D.inner =
  | Local of int
  | Global of Ident.path * D.value Lazy.t
  | Staged of Ident.path * D.outer Lazy.t * D.value Lazy.t

  | Lam of Ident.t * t
  | Ap of t * t

  | Quote of t
  | Splice of t

  | CodePi of t * t
  | CodeUniv of int
