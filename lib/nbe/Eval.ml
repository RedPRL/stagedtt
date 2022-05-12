open Core
open TermBuilder

module S = Syntax
module D = Domain
module I = Inner
module O = Outer

exception NbeFailed of string

open struct
  type env =
    { locals : D.env;
      stage : int }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let get_local ix =
    match D.Env.lookup_idx (Eff.read ()).locals ix with
    | Some v -> Lazy.force v
    | None -> raise @@ NbeFailed "Variable lookup out of scope!"

  let get_local_tp ix =
    match D.Env.lookup_tp_idx (Eff.read ()).locals ix with
    | Some tp -> tp
    | None -> raise @@ NbeFailed "Variable lookup out of scope!"

  let clo body =
    D.Clo (body, (Eff.read()).locals)

  let eval_inner tm =
    let tm_stage = (Eff.read ()).stage in
    Stage.eval_inner ~tm_stage tm

  let with_locals locals k =
    Eff.scope (fun env -> { env with locals }) k

  (** {1 Evaluating Terms} *)

  let rec eval : S.t -> D.t =
    function
    | S.Local ix ->
      get_local ix
    | S.Global (`Unstaged (path, v, inner)) ->
      D.global path v inner
    | S.Global (`Staged (_, v, _, _)) ->
      Lazy.force v
    | S.Lam (x, body) ->
      D.Lam (x, clo body)
    | S.Ap (f, a) ->
      do_ap (eval f) (eval a) (eval_inner a)
    | S.Quote t ->
      D.Quote (eval t)
    | S.Splice (t) ->
      do_splice (eval t)
    | S.CodePi (base, fam) ->
      D.Code (D.CodePi (eval base, eval fam))
    | S.CodeUniv stage ->
      D.Code (D.CodeUniv stage)

  and eval_fields fields =
    List.map (fun (lbl, t) -> (lbl, eval t)) fields

  (** {1 Evaluating Types} *)

  and eval_tp : S.tp -> D.tp =
    function
    | S.TpVar ix ->
      get_local_tp ix
    | S.Pi (base, x, fam) ->
      D.Pi (eval_tp base, x, clo fam)
    | S.Expr tp ->
      D.Expr (eval_tp tp)
    | S.El code ->
      do_el (eval code)
    | S.Univ stage ->
      D.Univ stage

  (** {1 Eliminators} *)

  and do_ap (v : D.t) (arg : D.t) (iarg : I.t) =
    match v with
    | D.Lam (_, clo) ->
      inst_tm_clo clo arg
    | D.Neu neu ->
      let unfold fn = do_ap fn arg iarg in
      let stage fn = I.Ap (fn, iarg) in
      D.Neu (D.push_frm neu (D.Ap arg) ~unfold ~stage)
    | _ ->
      raise @@ NbeFailed "Not a function in do_ap"

  and do_splice (v : D.t) =
    match v with
    | D.Quote t ->
      t
    | D.Neu neu ->
      let unfold = do_splice in
      let stage tm = I.Splice tm in
      D.Neu (D.push_frm neu D.Splice ~unfold ~stage)
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

  and unfold_el (code : D.code) : D.tp =
    match code with
    | D.CodePi (base, fam) ->
      graft_tp @@
      Graft.value base @@ fun base ->
      Graft.value fam @@ fun fam ->
      Graft.build @@
      TB.pi (TB.el base) @@ fun x -> TB.el (TB.ap fam x)
    | D.CodeUniv stage -> D.Univ stage

  (** {1 Closure Instantiation} *)

  and inst_tm_clo (clo : D.tm_clo) (x : D.t) : D.t =
    match clo with
    | D.Clo (body, env) ->
      with_locals (D.Env.extend env x) @@ fun () -> eval body

  and inst_tp_clo (clo : D.tp_clo) (x : D.t) : D.tp =
    match clo with
    | D.Clo (body, env) ->
      with_locals (D.Env.extend env x) @@ fun () ->
      eval_tp body

  and graft_value (gtm : S.t Graft.t) =
    let tm, env = Graft.graft gtm in
    with_locals env @@ fun () -> eval tm

  and graft_tp (gtp : S.tp Graft.t) =
    let tp, env = Graft.graft gtp in
    with_locals env @@ fun () -> eval_tp tp
end

(** {1 Public Interface} *)

(* [TODO: Reed M, 28/04/2022] Can we use Lazy.force_val here? *)
let unfold : D.t -> D.t =
  function
  | D.Neu { hd = D.Global(`Unstaged (_, v, _)); _ } -> Lazy.force v
  | tm -> tm

let eval ~stage ~env tm =
  let env = { stage; locals = env } in
  Eff.run ~env @@ fun () -> eval tm

let eval_tp ~stage ~env tp =
  let env = { stage; locals = env } in
  Eff.run ~env @@ fun () -> eval_tp tp

let inst_tm_clo = inst_tm_clo
let inst_tp_clo = inst_tp_clo

let do_ap = do_ap
let do_splice = do_splice
let do_el = do_el

let unfold_el = unfold_el

let graft_value = graft_value
let graft_tp = graft_tp