module S = Syntax
module D = Domain

open struct
  module Eff = Algaeff.Reader.Make (struct type env = int end)

  let bind_var (f : D.t -> 'a) : 'a =
    Eff.scope (fun size -> size + 1) @@ fun () ->
    let size = Eff.read () in 
    f (D.local (size - 1))

  let quote_var lvl =
    let n = Eff.read () in
    n - (lvl + 1)

  (*******************************************************************************
   * Quoting Values *)

  let rec quote : D.t -> S.t =
    function
    | D.Neu neu ->
      quote_neu neu
    | D.Lam (x, clo) ->
      S.Lam (x, quote_tm_clo clo)
    | D.Struct fields ->
      S.Struct (quote_fields fields)
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
    | D.Sign sign ->
      S.Sign (quote_sign sign)
    | D.Univ ->
      S.Univ
    | D.Expr tp ->
      S.Expr (quote_tp tp)
    | D.El code ->
      S.El (quote_code code)
    | D.ElNeu neu ->
      S.El (quote_neu neu)

  and quote_sign : D.sign -> S.sign =
    function
    | D.Empty -> []
    | D.Field (lbl, tp, clo) -> (lbl, quote_tp tp) :: quote_sign_clo clo

  (*******************************************************************************
   * Quoting Codes *)

  and quote_code : D.code -> S.t =
    function
    | D.CodePi (base, fam) ->
      S.CodePi (quote base, quote fam)
    | D.CodeSign fields -> 
      S.CodeSign (quote_fields fields)
    | D.CodeUniv ->
      S.CodeUniv

  (*******************************************************************************
   * Quoting Neutrals *)

  and quote_neu (neu : D.neu) : S.t =
    let tm = quote_hd neu.hd in
    quote_spine tm neu.spine

  and quote_hd : D.hd -> S.t =
    function
    | D.Local lvl -> S.Local (quote_var lvl)
    | D.Global (nm, v) -> S.Global (nm, v)

  and quote_spine (tm : S.t) : D.frm list -> S.t =
    function
    | [] -> tm
    | (frm :: spine) -> quote_spine (quote_frm tm frm) spine 

  and quote_frm (tm : S.t) : D.frm -> S.t =
    function
    | D.Ap arg ->
      let arg = quote arg in
      S.Ap (tm, arg)
    | D.Proj lbl ->
      S.Proj (tm, lbl)
    | D.Splice ->
      S.Splice tm


  (*******************************************************************************
   * Quoting Closures *)

  and quote_tm_clo (clo : D.tm_clo) : S.t =
    bind_var @@ fun arg -> quote (Eval.inst_tm_clo clo arg)

  and quote_tp_clo (clo : D.tp_clo) : S.tp =
    bind_var @@ fun arg -> quote_tp (Eval.inst_tp_clo clo arg)

  and quote_sign_clo (clo : D.sign_clo) : S.sign =
    bind_var @@ fun arg -> quote_sign (Eval.inst_sign_clo clo arg)
end

(*******************************************************************************
 * Public Interface *)

let quote ~(size : int) (tm : D.t) : S.t =
  Eff.run ~env:size @@ fun () -> quote tm

let quote_tp ~(size : int) (tp : D.tp) : S.tp =
  Eff.run ~env:size @@ fun () -> quote_tp tp
