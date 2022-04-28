open Bwd

module S = Syntax
module D = Domain

exception NbeFailed of string 

open struct
  module Eff = Algaeff.Reader.Make (struct type env = D.env end)

  let get_local ix =
    match BwdLabels.nth_opt (Eff.read ()).venv ix with
    | Some v -> v
    | None -> raise @@ NbeFailed "Variable out of scope!"

  let get_local_tp ix =
    match BwdLabels.nth_opt (Eff.read ()).tpenv ix with
    | Some v -> v
    | None -> raise @@ NbeFailed "Variable out of scope!"

  let clo body =
    D.Clo (body, Eff.read())

  and push_frm (neu : D.neu) (frm : D.frm) ~(unfold : D.t -> D.t) : D.neu =
    match neu.hd with
    | D.Global (nm, v) ->
      { hd = Global (nm, Lazy.map unfold v); spine = frm :: neu.spine }
    | _ ->
      { hd = neu.hd; spine = frm :: neu.spine }

  (*******************************************************************************
   * Evaluating Values *)

  let rec eval : S.t -> D.t =
    function
    | S.Local ix ->
      get_local ix
    | S.Global (nm, v) ->
      D.global nm v
    | S.Lam (x, body) ->
      D.Lam (x, clo body)
    | S.Ap (f, a) ->
      do_ap (eval f) (eval a)
    | S.Struct fields ->
      D.Struct (eval_fields fields)
    | S.Proj (t, lbl) ->
      do_proj (eval t) lbl
    | S.Quote t ->
      D.Quote (eval t)
    | S.Splice t ->
      do_splice (eval t)
    | S.CodePi (base, fam) ->
      D.Code (D.CodePi (eval base, eval fam))
    | S.CodeSign fields ->
      D.Code (D.CodeSign (eval_fields fields))
    | S.CodeUniv stage ->
      D.Code (D.CodeUniv stage)

  and eval_fields fields =
    List.map (fun (lbl, t) -> (lbl, eval t)) fields

  (*******************************************************************************
   * Evaluating Types *)

  and eval_tp : S.tp -> D.tp =
    function
    | S.TpVar ix ->
      get_local_tp ix
    | S.Pi (base, x, fam) ->
      D.Pi (eval_tp base, x, clo fam)
    | S.Sign sign ->
      D.Sign (eval_sign sign)
    | S.Expr tp ->
      D.Expr (eval_tp tp)
    | S.El code ->
      do_el (eval code)
    | S.Univ stage ->
      D.Univ stage

  and eval_sign : S.sign -> D.sign =
    function
    | [] -> D.Empty
    | (lbl, tp) :: sign -> D.Field (lbl, eval_tp tp, clo sign)

  (*******************************************************************************
   * Eliminators *)

  and do_ap (v : D.t) (arg : D.t) =
    match v with
    | D.Lam (_, clo) ->
      inst_tm_clo clo arg
    | D.Neu neu ->
      D.Neu (push_frm neu (D.Ap arg) ~unfold:(fun fn -> do_ap fn arg))
    | _ ->
      raise @@ NbeFailed "Not a function in do_ap"

  and do_proj (v : D.t) (lbl : string) =
    match v with
    | D.Struct fields ->
      List.assoc lbl fields
    | D.Neu neu ->
      D.Neu (push_frm neu (D.Proj lbl) ~unfold:(fun v -> do_proj v lbl))
    | _ ->
      raise @@ NbeFailed "Not a struct in do_proj"

  and do_splice (v : D.t) =
    match v with
    | D.Quote t ->
      t
    | D.Neu neu ->
      D.Neu (push_frm neu D.Splice ~unfold:do_splice)
    | _ ->
      raise @@ NbeFailed "Not a quoted value in do_splice"

  and do_el (v : D.t) : D.tp =
    match v with
    | D.Code code ->
      D.El code
    | D.Neu neu ->
      D.ElNeu neu
    | _ ->
      raise @@ NbeFailed "Not a code in do_el"

  (*******************************************************************************
   * Closure Instantiation *)

  and inst_tm_clo (clo : D.tm_clo) (x : D.t) : D.t =
    match clo with
    | D.Clo (body, env) -> Eff.run ~env:{ env with venv = Snoc(env.venv, x) } (fun () -> eval body)

  and inst_tp_clo (clo : D.tp_clo) (x : D.t) : D.tp =
    match clo with
    | D.Clo (body, env) -> Eff.run ~env:{ env with venv = Snoc(env.venv, x) } (fun () -> eval_tp body)

  and inst_sign_clo (clo : D.sign_clo) (x : D.t) : D.sign =
    match clo with
    | D.Clo (body, env) -> Eff.run ~env:{ env with venv = Snoc(env.venv, x) } (fun () -> eval_sign body)

end

(*******************************************************************************
 * Public Interface *)

let push_frm = push_frm

let eval ~env tm =
  Eff.run ~env @@ fun () -> eval tm

let eval_tp ~env tp =
  Eff.run ~env @@ fun () -> eval_tp tp

let inst_tm_clo = inst_tm_clo
let inst_tp_clo = inst_tp_clo
let inst_sign_clo = inst_sign_clo

let do_ap = do_ap
let do_proj = do_proj
let do_splice = do_splice
let do_el = do_el
