open Core
open Eff

module S = Syntax
module D = Domain


(*******************************************************************************
 * Quoting Values *)

let rec quote : D.t -> S.t =
  function
  | D.Neu neu ->
    quote_neu neu
  | D.Lam (x, clo) ->
    S.Lam (x, quote_tm_clo clo)
  | D.Quote v ->
    S.Quote (quote v)
  | D.Code code ->
    quote_code code

and quote_fields (fields : (string * D.t) list) : (string * S.t) list =
  List.map (fun (lbl, x) -> (lbl, quote x)) fields

(*******************************************************************************
 * Quoting Types *)

and quote_tp : D.tp -> S.tp =
  function
  | D.Pi (base, x, fam) ->
    S.Pi (quote_tp base, x, quote_tp_clo fam)
  | D.Univ stage ->
    S.Univ stage
  | D.Expr tp ->
    S.Expr (quote_tp tp)
  | D.El code ->
    S.El (quote_code code)
  | D.ElNeu neu ->
    S.El (quote_neu neu)

(*******************************************************************************
 * Quoting Codes *)

and quote_code : D.code -> S.t =
  function
  | D.CodePi (base, fam) ->
    S.CodePi (quote base, quote fam)
  | D.CodeExpr tm ->
    S.CodeExpr (quote tm)
  | D.CodeUniv stage ->
    S.CodeUniv stage

(*******************************************************************************
 * Quoting Neutrals *)

and quote_neu (neu : D.neu) : S.t =
  let tm = quote_hd neu.hd in
  quote_spine tm neu.spine

and quote_hd : D.hd -> S.t =
  function
  | D.Local lvl -> S.Local (Quoting.level_to_index lvl)
  | D.Global gbl -> S.Global (gbl :> S.global)
  | D.Hole nm -> S.Hole nm

and quote_spine (tm : S.t) : D.frm list -> S.t =
  function
  | [] -> tm
  | (frm :: spine) -> quote_spine (quote_frm tm frm) spine 

and quote_frm (tm : S.t) : D.frm -> S.t =
  function
  | D.Ap arg ->
    let arg = quote arg in
    S.Ap (tm, arg)
  | D.Splice ->
    S.Splice tm


(*******************************************************************************
 * Quoting Closures *)

and quote_tm_clo (clo : D.tm_clo) : S.t =
  Quoting.bind_var @@ fun arg -> quote (Eval.inst_tm_clo clo (D.local arg))

and quote_tp_clo (clo : D.tp_clo) : S.tp =
  Quoting.bind_var @@ fun arg -> quote_tp (Eval.inst_tp_clo clo (D.local arg))
