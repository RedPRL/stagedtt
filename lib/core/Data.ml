open Bwd

module rec Syntax : sig
  type t =
    | Local of int
    (** DeBruijin-Indexed variables. *)
    | Global of string * Domain.t Lazy.t

    | Lam of string * t
    | Ap of t * t

    | Struct of (string * t) list
    | Proj of t * string

    | Quote of t
    | Splice of t

    | CodePi of t * t
    | CodeSign of (string * t) list
    | CodeUniv of int

  and tp =
    | TpVar of int
    (** DeBruijin-Indexed type variables.
        These are mainly used during grafting. *)
    | Pi of tp * string * tp
    | Sign of sign
    | Expr of tp
    | El of t
    | Univ of int

  and sign = (string * tp) list
end =
struct
  type t =
    | Local of int
    | Global of string * Domain.t Lazy.t

    | Lam of string * t
    | Ap of t * t

    | Struct of (string * t) list
    | Proj of t * string

    | Quote of t
    | Splice of t

    | CodePi of t * t
    | CodeSign of (string * t) list
    | CodeUniv of int

  and tp =
    | TpVar of int
    | Pi of tp * string * tp
    | Sign of sign
    | Expr of tp
    | El of t
    | Univ of int

  and sign = (string * tp) list
end

and Domain : sig

  (** {2 Values} *)
  type t =
    | Lam of string * tm_clo
    | Struct of (string * t) list
    | Quote of t
    | Neu of neu
    | Code of code

  and code =
    | CodePi of t * t
    | CodeSign of (string * t) list
    | CodeUniv of int

  (** {2 Types} *)
  and tp =
    | Pi of tp * string * tp_clo
    | Sign of sign
    | Univ of int
    | Expr of tp
    | El of code
    | ElNeu of neu

  and sign =
    | Field of string * tp * Syntax.sign clo
    | Empty

  (** {2 Neutrals} *)
  and neu = { hd : hd; spine : frm list }

  and hd =
    | Local of int
    | Global of string * t Lazy.t

  and frm =
    | Ap of t
    | Proj of string
    | Splice

  (** {2 Closures} *)
  and 'a clo = Clo of 'a * env
  and tp_clo = Syntax.tp clo
  and tm_clo = Syntax.t clo
  and sign_clo = Syntax.sign clo

  (** {2 Environments} *)
  and env

  module Env : sig
    val empty : env

    (** Create an environment from a list of values
        NOTE: This also takes in the length of the list
        to avoid performing an O(n) computation. *)
    val from_vals : t Lazy.t bwd -> int -> env

    (** Lookup a DeBrujin Index in an environment. *)
    val lookup_idx : env -> int -> t Lazy.t option

    (** Lookup a DeBrujin Index in an environment. *)
    val lookup_tp_idx : env -> int -> tp option

    (** Get all the local bindings in an environment. *)
    val locals : env -> t Lazy.t bwd

    (** Get the number of values in the environment. *)
    val size : env -> int

    (** Get the number of types in the environment. *)
    val tp_size : env -> int

    (** Extend an environment with a value. *)
    val extend : env -> t -> env

    (** Extend an environment with a type. *)
    val extend_tp : env -> tp -> env
  end
end =
struct
  type t =
    | Lam of string * tm_clo
    | Struct of (string * t) list
    | Quote of t
    | Neu of neu
    | Code of code

  and code =
    | CodePi of t * t
    | CodeSign of (string * t) list
    | CodeUniv of int

  and tp =
    | Pi of tp * string * tp_clo
    | Sign of sign
    | Univ of int
    | Expr of tp
    | El of code
    | ElNeu of neu

  and sign =
    | Field of string * tp * Syntax.sign clo
    | Empty

  and neu = { hd : hd; spine : frm list }

  and hd =
    | Local of int
    | Global of string * t Lazy.t

  and frm =
    | Ap of t
    | Proj of string
    | Splice

  and 'a clo = Clo of 'a * env
  and tp_clo = Syntax.tp clo
  and tm_clo = Syntax.t clo
  and sign_clo = Syntax.sign clo

  (* [NOTE: Environment Sizes]
     We often have to compute the size of an environment when performing various grafting
     operations, which is O(n) for {!type:bwd}. Therefore, we store the length of the
     environment alongside   

  *)
  and env =
    { locals : (t Lazy.t) bwd;
      size : int;
      tp_locals : tp bwd;
      tp_size : int }

  module Env =
  struct
    let empty =
      { locals = Emp;
        size = 0;
        tp_locals = Emp;
        tp_size = 0 }

    let from_vals locals size =
      { locals;
        size;
        tp_locals = Emp;
        tp_size = 0 }

    let lookup_idx env idx =
      BwdLabels.nth_opt env.locals idx

    let lookup_tp_idx env idx =
      BwdLabels.nth_opt env.tp_locals idx

    let locals env =
      env.locals

    let size env =
      env.size

    let tp_size env =
      env.tp_size

    let extend env v =
      { env with
        locals = Snoc(env.locals, Lazy.from_val v);
        size = env.size + 1 }

    let extend_tp env tp =
      { env with
        tp_locals = Snoc(env.tp_locals, tp);
        tp_size = env.tp_size + 1 }
  end
end
