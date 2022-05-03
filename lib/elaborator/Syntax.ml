(* [TODO: Reed M, 02/05/2022] Update to use cells *)
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

let rec dump fmt =
  function
  | Ann {tm; tp} ->
    Format.fprintf fmt "ann[%a, %a]"
      dump tm
      dump tp
  | Var nm ->
    Format.fprintf fmt "var[%s]"
      nm
  | Pi (base, ident, fam) ->
    Format.fprintf fmt "pi[%s, %a, %a]"
      ident
      dump base
      dump fam
  | Lam (xs, body) ->
    Format.fprintf fmt "lam[%a, %a]"
      (Format.pp_print_list Format.pp_print_string) xs
      dump body
  | Ap (t, tms) ->
    Format.fprintf fmt "ap[%a, %a]"
      dump t
      (Format.pp_print_list dump) tms
  | Sign _ -> failwith "[FIXME] dump: Going to rework signs"
  | Struct _ -> failwith "[FIXME] dump: going to rework signs"
  | Proj (_, _) -> failwith "[FIXME] dump: going to rework signs"
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
