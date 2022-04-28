open Bwd

module rec Syntax : sig
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
    | CodeUniv

  and tp =
    | TpVar of int
    | Pi of tp * string * tp
    | Sign of sign
    | Expr of tp
    | El of t
    | Univ

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
    | CodeUniv

  and tp =
    | TpVar of int
    | Pi of tp * string * tp
    | Sign of sign
    | Expr of tp
    | El of t
    | Univ

  and sign = (string * tp) list
end

and Domain : sig
  type env = { tpenv : tp bwd; venv : t bwd }

  and 'a clo = Clo of 'a * env
  and tp_clo = Syntax.tp clo
  and tm_clo = Syntax.t clo
  and sign_clo = Syntax.sign clo

  and t =
    | Lam of string * tm_clo
    | Struct of (string * t) list
    | Quote of t
    | Neu of neu
    | Code of code

  and code =
    | CodePi of t * t
    | CodeSign of (string * t) list
    | CodeUniv

  and tp =
    | Pi of tp * string * tp_clo
    | Sign of sign
    | Univ
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
end =
struct
  type env = { tpenv : tp bwd; venv : t bwd }

  and 'a clo = Clo of 'a * env
  and tp_clo = Syntax.tp clo
  and tm_clo = Syntax.t clo
  and sign_clo = Syntax.sign clo

  and t =
    | Lam of string * tm_clo
    | Struct of (string * t) list
    | Quote of t
    | Neu of neu
    | Code of code

  and code =
    | CodePi of t * t
    | CodeSign of (string * t) list
    | CodeUniv

  and tp =
    | Pi of tp * string * tp_clo
    | Sign of sign
    | Univ
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
end
