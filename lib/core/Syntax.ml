open Bwd
open Data

module StringMap = Map.Make (String)

include Syntax

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
  | Struct _ -> Prec.juxtaposition
  | Proj _ -> Prec.proj
  | Quote _ -> Prec.quote
  | Splice _ -> Prec.splice
  | CodePi _ -> Prec.arrow
  | CodeSign _ -> Prec.juxtaposition
  | CodeUniv _ -> Prec.atom

let rec pp env =
  Pp.parens classify_tm env @@ fun fmt ->
  function
  | Local ix ->
    Pp.var env fmt ix
  | Global (nm, _) ->
    (* [TODO: Reed M, 02/05/2022] Support unfolding during pretty printing *)
    Format.pp_print_string fmt nm
  | Lam (x, body) ->
    let x, env = Pp.bind_var x env in
    Format.fprintf fmt "λ %s → %a"
      x
      (pp (Pp.right_of Prec.arrow env)) body
  | Ap (f, arg) ->
    Format.fprintf fmt "%a %a"
      (pp (Pp.left_of Prec.juxtaposition env)) f
      (pp (Pp.right_of Prec.juxtaposition env)) arg
  | Struct _ -> failwith "[FIXME] pp: Structs are going to be reworked, no sense bothering"
  | Proj _ -> failwith "[FIXME] pp: Structs are going to be reworked, no sense bothering"
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
  | CodeSign _ -> failwith "[FIXME] pp: Structs are going to be reworked, no sense bothering"
  | CodeUniv stage ->
    Format.fprintf fmt "type %d" stage

let rec dump fmt =
  function
  | Local ix ->
    Format.fprintf fmt "var[%d]"
      ix
  | Global (nm, _) ->
    Format.fprintf fmt "global[%s]"
      nm
  | Lam (_, body) ->
    Format.fprintf fmt "lam[%a]"
      dump body
  | Ap (f, a) ->
    Format.fprintf fmt "ap[%a, %a]"
      dump f
      dump a
  | Struct _ -> failwith "[FIXME] dump: Structs will be reworked"
  | Proj (_, _) -> failwith "[FIXME] dump: Structs will be reworked"
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
  | CodeSign _ -> failwith "[FIXME] dump: Structs will be reworked"
  | CodeUniv stage ->
    Format.fprintf fmt "type[%d]"
      stage
