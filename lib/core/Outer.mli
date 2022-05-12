open Prelude
module D := Data

type env = D.stage_env

type staged = D.staged =
  | Inner of D.inner
  | Outer of t

and 'a clo = 'a D.sclo = Clo of 'a * env
and tm_clo = D.syn clo

and t = D.outer =
  | Lam of Ident.t * tm_clo
  | Quote of D.inner
  | Irrelevant

module Env : sig
  val empty : env
  val extend_inner : env -> D.inner -> env
  val extend_outer : env -> D.outer -> env

  val lookup_lvl : env -> int -> staged

  val size : env -> int
end
