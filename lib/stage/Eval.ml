open Prelude
open Core

module S = Syntax
module I = Inner
module O = Outer


open struct
  type env =
    { locals : O.env;
      goal_stage : int;
      current_stage : int }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)


  let impossible msg =
    Format.asprintf msg
    |> Diagnostic.impossible ~code:"XSTAGE"
    |> Diagnostic.fatal

  let get_locals () =
    (Eff.read()).locals

  let get_outer_local ix =
    let env = get_locals () in
    match O.Env.lookup_lvl env ix with
    | O.Outer otm -> otm
    | O.Inner _ ->
      impossible "Expected an outer variable in 'get_outer_local'"

  let get_inner_local ix =
    let env = get_locals () in
    match O.Env.lookup_lvl env ix with
    | O.Inner itm -> itm
    | O.Outer _ ->
      impossible "Expected an inner variable in 'get_inner_local'"

  type mode =
    | Inner
    | Outer

  let incr_stage k =
    let env = Eff.read () in
    let mode =
      if env.current_stage + 1 > env.goal_stage then Outer else Inner
    in
    Eff.scope (fun env -> { env with current_stage = env.current_stage + 1 }) (fun () -> k mode)

  let decr_stage k =
    let env = Eff.read () in
    let mode =
      if env.current_stage - 1 <= env.goal_stage then Inner else Outer
    in
    Eff.scope (fun env -> { env with current_stage = env.current_stage - 1 }) (fun () -> k mode)

  let bind_inner k =
    let extend env =
      let var = I.Local (O.Env.size env.locals) in
      let locals = O.Env.extend_inner env.locals var in
      { env with locals }
    in
    Eff.scope extend k

  let with_locals locals k =
    Eff.scope (fun env -> { env with locals }) k

  let clo body =
    O.Clo (body, get_locals ())


  (* Evaluates all redexes, and return a piece of syntax of stage 'stage'. *)
  let rec eval_outer (tm : S.t) =
    match tm with
    | S.Local ix ->
      get_outer_local ix
    | S.Global (_, _) ->
      impossible "Encountered a global variable of stage 0 when evaluating outer syntax."
    | S.Staged (_, unf, _) ->
      (* [NOTE: Outer Staged Globals]
         If we encounter a staged global at this point, we know that
         it's stage is greater than our goal stage that we are evaluating to.
         Therefore, the only sensible thing to do here is to unfold it's outer
         syntax representation.

         [TODO: Reed M, 11/05/2022]
         In theory, we may not need to force an unfolding here, but this works for now.
         For instance, if we have something like

           \(x y -> x) ↑[...] some_global

         Then we don't need to unfold 'some_global' at all. The fix here is to 
         introduce a 'Staged' constructor to the outer values, and then lazily
         apply eliminators, and only force the value if we actually need it. *)
      Lazy.force unf
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
    | S.CodeUniv _ ->
      O.Irrelevant

  and eval_inner (tm : S.t) =
    match tm with
    | S.Local ix ->
      get_inner_local ix
    | S.Global (path, unfold) ->
      (* [NOTE: Unfolding Globals in Staging]
         It may seem like we need to unfold this global, but recall
         that all globals are at stage 0. As their unfoldings have already
         been evaluated, all redexes at will already have been evaluated! *)
      I.Global (path, unfold)
    | S.Staged (path, sunf, vunf) ->
      (* [NOTE: Inner Staged Globals]
         Because we've already typechecked by this point, we are safe to assume
         that this globals stage is less than or equal to the goal stage that
         we are evaluating to. This means that we don't need to perform any
         unfoldings. *)
      I.Staged (path, sunf, vunf)
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
    | S.CodeUniv stage -> I.CodeUniv stage

  and do_outer_ap fn a =
    match fn with
    | O.Lam (_, clo) -> inst_tm_clo clo a
    | _ -> 
      impossible "Expected a function in do_outer_ap"

  and do_splice (otm : O.t) =
    match otm with
    | O.Quote tm -> tm
    | _ -> impossible "Expected a quoted inner value in do_splice"

  and inst_tm_clo clo v =
    match clo with
    | O.Clo (body, env) ->
      with_locals (O.Env.extend_outer env v) @@ fun () ->
      eval_outer body

end

let eval_inner ~stage ~tm_stage tm =
  let env = {
    locals = O.Env.empty;
    goal_stage = stage;
    current_stage = tm_stage }
  in Eff.run ~env @@ fun () -> eval_inner tm
