open Prelude

(* Disable the warning for duplicate constructors *)
[@@@warning "-30"]

(** {1 Syntax} *)

type syn =
  | Local of int
  | Global of Ident.path * value Lazy.t
  | Staged of Ident.path * outer Lazy.t * value Lazy.t

  | Lam of Ident.t * syn
  | Ap of syn * syn

  | Quote of syn
  | Splice of syn

  | CodePi of syn * syn
  | CodeUniv of int

and syn_tp =
  | TpVar of int
  (** DeBruijin-Indexed type variables.
      These are used during grafting. *)
  | Pi of syn_tp * Ident.t * syn_tp
  | Expr of syn_tp
  | El of syn
  | Univ of int

(** {1 Values} *)

and value =
  | Lam of Ident.t * tm_vclo
  | Quote of value
  | Neu of neu
  | Code of code

and code =
  | CodePi of value * value
  | CodeUniv of int

and value_tp =
  | Pi of value_tp * Ident.t * tp_vclo
  | Expr of value_tp
  | El of code
  | ElNeu of neu
  | Univ of int

and neu = { hd : hd; spine : frm list }

and hd =
  | Local of int
  | Global of Ident.path * value Lazy.t

and frm =
  | Ap of value
  | Splice

and 'a vclo =
  | Clo of 'a * value_env
and tp_vclo = syn_tp vclo
and tm_vclo = syn vclo

and value_env = 
  { locals : (value Lazy.t) bwd;
    size : int;
    tp_locals : value_tp bwd;
    tp_size : int
  }

(** {1 Staging} *)

and outer =
  | Lam of Ident.t * tm_sclo
  | Quote of inner
  | Irrelevant

and inner =
  | Local of int
  | Global of Ident.path * value Lazy.t
  | Staged of Ident.path * outer Lazy.t * value Lazy.t

  | Lam of Ident.t * inner
  | Ap of inner * inner

  | Quote of inner
  | Splice of inner

  | CodePi of inner * inner
  | CodeUniv of int

and staged =
  | Inner of inner
  | Outer of outer

and stage_env =
  { locals : staged bwd;
    size : int
  }

and 'a sclo = Clo of 'a * stage_env
and tm_sclo = syn sclo
