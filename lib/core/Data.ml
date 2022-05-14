open Prelude

(* Disable the warning for duplicate constructors *)
[@@@warning "-30"]

(** {1 Syntax} *)

type global =
  [ `Unstaged of Ident.path * value Lazy.t * inner Lazy.t
  | `Staged of Ident.path * value Lazy.t * inner Lazy.t * (int -> outer)
  ]

and syntax =
  | Local of int
  | Global of global
  | Hole of string option

  | Lam of Ident.t * syntax
  | Ap of syntax * syntax

  | Quote of syntax
  | Splice of syntax

  | CodePi of syntax * syntax
  | CodeUniv of int

and syntax_tp =
  | TpVar of int
  (** DeBruijin-Indexed type variables.
      These are used during grafting. *)
  | Pi of syntax_tp * Ident.t * syntax_tp
  | Expr of syntax_tp
  | El of syntax
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
  | Global of [ `Unstaged of Ident.path * value Lazy.t * inner Lazy.t ]
  | Hole of string option


and frm =
  | Ap of value
  | Splice

and 'a vclo =
  | Clo of 'a * value_env
and tp_vclo = syntax_tp vclo
and tm_vclo = syntax vclo

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
  | Global of global
  | Hole of string option

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
and tm_sclo = syntax sclo
