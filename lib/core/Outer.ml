open Prelude
module D = Data

type staged = D.staged =
  | Inner of D.inner
  | Outer of t

and env = D.stage_env =
  { locals : staged bwd;
    size : int }

and 'a clo = 'a D.sclo = Clo of 'a * env
and tm_clo = D.syntax clo

and t = D.outer =
  | Lam of Ident.t * tm_clo
  | Quote of D.inner
  | Irrelevant

module Env =
struct
  let empty =
    { locals = Emp;
      size = 0 }

  let extend_inner env tm =
    { locals = Snoc(env.locals, Inner tm); size = env.size + 1 }

  let extend_outer env tm =
    { locals = Snoc(env.locals, Outer tm); size = env.size + 1 }

  let lookup_lvl env ix =
    Bwd.nth env.locals ix

  let size env =
    env.size

  let from_vals locals size =
    { locals; size }
end
