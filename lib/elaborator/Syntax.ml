open Prelude

(* [TODO: Reed M, 02/05/2022] Update to use cells *)
type t =
  | Ann of { tm : t; tp : t }
  | Var of Ident.path 
  | Pi of t * Ident.t * t
  | Lam of Ident.t list * t
  | Ap of t * t list
  | Expr of t
  | Quote of t
  | Splice of t
  | Univ of { stage : int }

let rec dump fmt =
  function
  | Ann {tm; tp} ->
    Format.fprintf fmt "ann[%a, %a]"
      dump tm
      dump tp
  | Var ident ->
    Format.fprintf fmt "var[%a]"
      Ident.pp_path ident
  | Pi (base, ident, fam) ->
    Format.fprintf fmt "pi[%a, %a, %a]"
      Ident.pp ident
      dump base
      dump fam
  | Lam (xs, body) ->
    Format.fprintf fmt "lam[%a, %a]"
      (Format.pp_print_list Ident.pp) xs
      dump body
  | Ap (t, tms) ->
    Format.fprintf fmt "ap[%a, %a]"
      dump t
      (Format.pp_print_list dump) tms
  | Expr tm ->
    Format.fprintf fmt "expr[%a]"
      dump tm
  | Quote tm ->
    Format.fprintf fmt "quote[%a]"
      dump tm
  | Splice tm ->
    Format.fprintf fmt "splice[%a]"
      dump tm
  | Univ {stage} ->
    Format.fprintf fmt "type[%d]"
      stage
