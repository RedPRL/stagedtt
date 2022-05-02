type t =
  | Ann of { tm : t; tp : t }
  | Var of string
  | Pi of t * string * t
  | Lam of string list * t
  | Ap of t * t list
  | Sign of (string * t) list
  | Struct of (string * t) list
  | Proj of t * string
  | Expr of t
  | Quote of t
  | Splice of t
  | Univ of { stage : int }
