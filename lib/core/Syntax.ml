open Prelude
module D = Data

module StringMap = Map.Make (String)

type global =
  [ `Unstaged of Ident.path * D.value Lazy.t * D.inner Lazy.t
  | `Staged of Ident.path * D.value Lazy.t * D.inner Lazy.t * (int -> D.outer)
  ]

and t = D.syntax =
  | Local of int
  | Global of global

  | Lam of Ident.t * t
  | Ap of t * t

  | Quote of t
  | Splice of t

  | CodePi of t * t
  | CodeUniv of int

type tp = D.syntax_tp =
  | TpVar of int
  | Pi of tp * Ident.t * tp
  | Expr of tp
  | El of t
  | Univ of int

let app f a = Ap (f, a) 
let apps f args = List.fold_left app f args

(** {1 Pretty Printing} *)

module Prec =
struct
  include Pp.Prec

  let passed = nonassoc 8
  let atom = nonassoc 7
  let delimited = nonassoc 6
  let juxtaposition = left 5
  let proj = right 4
  let arrow = right 3
  (* [TODO: Reed M, 02/05/2022] Figure out these *)
  let quote = right 3
  let splice = right 3
  let colon = nonassoc 2
  let arrow = right 1
  let in_ = nonassoc 0
end

let classify_tm =
  function
  | Local _ -> Prec.atom
  | Global _ -> Prec.atom
  | Lam _ -> Prec.arrow
  | Ap _ -> Prec.juxtaposition
  | Quote _ -> Prec.quote
  | Splice _ -> Prec.splice
  | CodePi _ -> Prec.arrow
  | CodeUniv _ -> Prec.atom

let classify_tp =
  function
  | TpVar _ -> Prec.atom
  | Pi _ -> Prec.arrow
  | Expr _ -> Prec.delimited
  | El _ -> Prec.passed
  | Univ _ -> Prec.atom

let rec pp env =
  Pp.parens classify_tm env @@ fun fmt ->
  function
  | Local ix ->
    Pp.var env fmt ix
  | Global (`Unstaged (name, _, _))
  | Global (`Staged (name, _, _, _)) ->
    Ident.pp_path fmt name
  | Lam (x, body) ->
    let x, env = Pp.bind_var x env in
    Format.fprintf fmt "λ %s → %a"
      x
      (pp (Pp.right_of Prec.arrow env)) body
  | Ap (f, arg) ->
    Format.fprintf fmt "%a %a"
      (pp (Pp.left_of Prec.juxtaposition env)) f
      (pp (Pp.right_of Prec.juxtaposition env)) arg
  | Quote tm ->
    Format.fprintf fmt "↑[ %a ]"
      (pp (Pp.isolated env)) tm
  | Splice tm ->
    Format.fprintf fmt "↓[ %a ]"
      (pp (Pp.isolated env)) tm
  | CodePi (base, fam) ->
    Format.fprintf fmt "Π %a %a"
      (pp (Pp.right_of Prec.juxtaposition env)) base
      (pp (Pp.right_of Prec.juxtaposition env)) fam
  | CodeUniv stage ->
    Format.fprintf fmt "type %d" stage

let rec pp_tp env =
  Pp.parens classify_tp env @@ fun fmt ->
  function
  | TpVar lvl ->
    Format.fprintf fmt "tpvar[%d]"
      lvl
  | Pi (base, ident, fam) ->
    let x, envx = Pp.bind_var ident env in
    Format.fprintf fmt "(%s : %a) → %a"
      x
      (pp_tp (Pp.left_of Prec.colon envx)) base
      (pp_tp (Pp.right_of Prec.arrow envx)) fam
  | Expr tp ->
    Format.fprintf fmt "⇑[ %a ]"
      (pp_tp (Pp.isolated env)) tp
  | El tp ->
    pp env fmt tp
  | Univ stage ->
    Format.fprintf fmt "type %d"
      stage

let rec dump fmt : t -> unit =
  function
  | Local ix ->
    Format.fprintf fmt "var[%d]"
      ix
  | Global (`Unstaged (nm, _, _)) ->
    Format.fprintf fmt "global[%a]"
      Ident.pp_path
      nm
  | Global (`Staged (nm, _, _, _)) ->
    Format.fprintf fmt "staged[%a]"
      Ident.pp_path
      nm
  | Lam (_, body) ->
    Format.fprintf fmt "lam[%a]"
      dump body
  | Ap (f, a) ->
    Format.fprintf fmt "ap[%a, %a]"
      dump f
      dump a
  | Quote tm ->
    Format.fprintf fmt "quote[%a]"
      dump tm
  | Splice tm ->
    Format.fprintf fmt "splice[%a]"
      dump tm
  | CodePi (base, fam) ->
    Format.fprintf fmt "code-pi[%a, %a]"
      dump base
      dump fam
  | CodeUniv stage ->
    Format.fprintf fmt "type[%d]"
      stage

and dump_tp fmt : tp -> unit =
  function
  | TpVar ix ->
    Format.fprintf fmt "tpvar[%d]"
      ix
  | Pi (base, ident, fam) ->
    Format.fprintf fmt "pi[%a, %a, %a]"
      dump_tp base
      Ident.pp ident
      dump_tp fam
  | Expr tp ->
    Format.fprintf fmt "expr[%a]"
      dump_tp tp
  | El tm ->
    Format.fprintf fmt "el[%a]"
      dump tm
  | Univ stage ->
    Format.fprintf fmt "univ[%d]"
      stage
