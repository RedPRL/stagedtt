open Prelude

type info = Span.t option

type 'a node =
  {node : 'a;
   info : info}

type t = t_ node
and t_ =
  | Ann of { tm : t; tp : t }
  | Var of Ident.path 
  | Hole of string option

  | Pi of t * Ident.t * t
  | Lam of Ident.t list * t
  | Ap of t * t list


  | Expr of t
  | Quote of t
  | Splice of t

  | Univ of { stage : int }

let rec dump fmt tm =
  match tm.node with
  | Ann {tm; tp} ->
    Format.fprintf fmt "ann[%a, %a]"
      dump tm
      dump tp
  | Var ident ->
    Format.fprintf fmt "var[%a]"
      Ident.pp_path ident
  | Hole nm ->
    Format.fprintf fmt "hole[%a]"
      (Format.pp_print_option Format.pp_print_string) nm
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
