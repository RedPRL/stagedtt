open Prelude

module S = Syntax
module D = Domain

(** A {!type:'a tb} can be thought of a piece of syntax that is
    relative to some environment. *)
type 'a tb = int -> 'a

let run_tb (env : D.env) (k : 'a tb) : 'a =
  k (D.Env.size env)

(** Convert a DeBruijin level into a DeBruijin indexed variable relative to the environment. *)
let var (lvl : int) : S.t tb =
  fun size ->
  S.Local (size - lvl - 1)

(** Convert a DeBruijin level into a DeBruijin indexed type variable relative to the environment. *)
let tp_var (lvl : int) : S.tp tb =
  fun size ->
  S.TpVar (size - lvl - 1)

let bind_var (k : int -> 'a tb) : 'a tb =
  fun size ->
  k size (size + 1)

let scope (k : S.t tb -> 'a tb) : 'a tb =
  bind_var (fun lvl -> k (var lvl))

(** {1 Term Builders} *)
module TB =
struct
  (** {2 Pi} *)
  let pi ?(ident = Ident.Anon) base body =
    fun env ->
      S.Pi (base env, ident, scope body env)

  let lam ?(ident = Ident.Anon) body =
    fun env ->
    S.Lam (ident, scope body env)

  let ap fn arg =
    fun env ->
    S.Ap (fn env, arg env)

  (** {2 Universes} *)
  let el t =
    fun env ->
    S.El (t env)
end

(** {1 Grafting} *)
module Graft =
struct
  type 'a t = D.env -> 'a tb * D.env

  let value (v : D.t) (k : S.t tb -> 'a t) : 'a t =
    fun env ->
    (* Create a variable that points to the end of the extended context.
       The DeBruijin arithmetic is a little tricky, but lets us avoid a subtraction. *)
    let x = var (D.Env.size env) in
    let env = D.Env.extend env v in
    k x env 

  let tp (tp : D.tp) (k : S.tp tb -> 'a t) : 'a t =
    fun env ->
    (* See {!val:value} for DeBruijin arithmetic trick. *)
    let x = tp_var (D.Env.tp_size env) in
    let env = D.Env.extend_tp env tp in
    k x env

  let fields xs k =
    let rec go fields k =
      match fields with
      | [] -> k []
      | (lbl, field) :: fields ->
        value field @@ fun field ->
        go fields @@ fun fields ->
        k @@ (lbl, field) :: fields
    in go xs k

  let build (builder : 'a tb) : 'a t =
    fun env -> (builder, env)

  let graft (k : 'a t) : 'a * D.env =
    let (tb, env) = k D.Env.empty in
    (run_tb env tb , env)
end
