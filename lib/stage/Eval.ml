open Core
open Eff

module S = Syntax
module D = Domain
module I = Inner
module O = Outer

open struct
  type env =
    { locals : O.env;
      stage : int; }

  module Reader = Algaeff.Reader.Make (struct type nonrec env = env end)

  let get_locals () =
    (Reader.read()).locals

  let get_outer_local ix =
    let env = get_locals () in
    match O.Env.lookup_lvl env ix with
    | O.Outer otm -> otm
    | O.Inner _ ->
      Doctor.impossible "Expected an outer variable in 'get_outer_local'"

  let get_inner_local ix =
    let env = get_locals () in
    match O.Env.lookup_lvl env ix with
    | O.Inner itm -> itm
    | O.Outer _ ->
      Doctor.impossible "Expected an inner variable in 'get_inner_local'"

  let incr_stage k =
    let stage = (Reader.read ()).stage in
    Staging.incr_stage stage k

  let decr_stage k =
    let stage = (Reader.read ()).stage in
    Staging.decr_stage stage k

  let bind_inner k =
    let extend env =
      let var = I.Local (O.Env.size env.locals) in
      let locals = O.Env.extend_inner env.locals var in
      { env with locals }
    in
    Reader.scope extend k

  let expand_outer_global (gbl : S.global) =
    match gbl with
    | `Unstaged _ ->
      Doctor.impossible "Encountered a global variable of stage 0 when evaluating outer syntax."
    | `Staged (_, _, _, expand) ->
      let stage = (Reader.read ()).stage in
      expand stage

  let with_locals locals k =
    Reader.scope (fun env -> { env with locals }) k

  let clo body =
    O.Clo (body, get_locals ())

  (* Evaluates all redexes, and return a piece of syntax of stage 'stage'. *)
  let rec eval_outer (tm : S.t) =
    match tm with
    | S.Local ix ->
      get_outer_local ix
    | S.Global gbl ->
      expand_outer_global gbl
    | S.Hole _ ->
      Doctor.impossible "Cannot have holes in metaprograms!"
    | S.Lam (x, body) ->
      O.Lam (x, clo body)
    | S.Ap (f, a) ->
      do_outer_ap (eval_outer f) (eval_outer a)
    | S.Quote tm ->
      decr_stage @@
      begin
        function
        | Outer -> eval_outer tm
        | Inner -> O.Quote (eval_inner tm)
      end
    | S.Splice tm ->
      incr_stage @@ fun _ -> eval_outer tm
    | S.CodePi _ ->
      O.Irrelevant
    | S.CodeExpr _ ->
      O.Irrelevant
    | S.CodeUniv _ ->
      O.Irrelevant

  and eval_inner (tm : S.t) =
    match tm with
    | S.Local ix ->
      get_inner_local ix
    | S.Global gbl ->
      I.Global gbl
    | S.Hole nm ->
      I.Hole nm
    | S.Lam (x, body) ->
      I.Lam (x, bind_inner @@ fun () -> eval_inner body)
    | S.Ap (fn, a) ->
      I.Ap (eval_inner fn, eval_inner a)
    | S.Quote tm ->
      I.Quote (decr_stage @@ fun _ -> eval_inner tm)
    | S.Splice tm ->
      incr_stage @@
      begin
        function
        | Outer -> do_splice (eval_outer tm)
        | Inner -> I.Splice (eval_inner tm)
      end
    | S.CodePi (base, fam) ->
      I.CodePi (eval_inner base, eval_inner fam)
    | S.CodeExpr tm ->
      I.CodeExpr (eval_inner tm)
    | S.CodeUniv stage -> I.CodeUniv stage

  and do_outer_ap fn a =
    match fn with
    | O.Lam (_, clo) -> inst_tm_clo clo a
    | _ -> 
      Doctor.impossible "Expected a function in do_outer_ap"

  and do_splice (otm : O.t) =
    match otm with
    | O.Quote tm -> tm
    | _ -> Doctor.impossible "Expected a quoted inner value in do_splice"

  and inst_tm_clo clo v =
    match clo with
    | O.Clo (body, env) ->
      with_locals (O.Env.extend_outer env v) @@ fun () ->
      eval_outer body
end

let eval_inner tm =
  let stage = Staging.get_stage () in
  let env = { locals = O.Env.empty; stage } in
  Reader.run ~env @@ fun () -> eval_inner tm

let eval_outer tm to_stage =
  let env = { locals = O.Env.empty; stage = to_stage } in
  Reader.run ~env @@ fun () -> eval_outer tm
