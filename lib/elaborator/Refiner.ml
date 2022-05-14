open Prelude
open Eff

module Ctx = OrderedHashTbl.Make(Ident)

module CS = Syntax

open Core

module S = Core.Syntax
module D = Core.Domain
module O = Core.Outer

(** {1 Effects} *)
type _ Effect.t +=
  | Resolve : Yuujinchou.Trie.path -> (S.global * int * D.tp) Effect.t

open struct
  type cell = { tp : D.tp; stage : int; level : int }

  type env = 
    { names : cell Ctx.t;
      values : D.env;
      has_holes : bool;
      strict_mode : bool
    }

  let create_env ?(strict_mode = false) size =
    let names = Ctx.create size in
    let values = D.Env.empty in
    { names; values; strict_mode; has_holes = false }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  (** {2 Variable Resolution} *)

  let resolve_global nm =
    Effect.perform (Resolve nm)

  let resolve_local ident =
    Ctx.find_idx_of (User ident) (Eff.read ()).names

  let get_local idx =
    snd @@ Ctx.nth idx (Eff.read ()).names

  (** {2 Variable Binding} *)

  let bind_var name stage tp k =
    let ctx = (Eff.read ()).names in
    let level = Ctx.size ctx in
    let value = D.local level in
    Eff.scope (fun env -> { env with values = D.Env.extend env.values value }) @@ fun () ->
    Ctx.scope name { tp; stage; level } ctx @@ fun () ->
    k value

  let clo body =
    let values = (Eff.read ()).values in
    D.Clo (body, values)

  (** {1 Wrappers for NbE} *)

  let lift_eval (f : env:D.env -> 'a) : 'a =
    let env = (Eff.read ()).values in
    f ~env

  let lift_quote (f : size:int -> 'a) : 'a =
    let ctx = (Eff.read ()).names in
    f ~size:(Ctx.size ctx)

  let eval ~stage tm = lift_eval (NbE.eval ~stage) tm
  let eval_tp ~stage tp = lift_eval (NbE.eval_tp ~stage) tp
  let quote tm = lift_quote NbE.quote tm
  let quote_tp tp = lift_quote NbE.quote_tp tp

  let inst_tm_clo = NbE.inst_tm_clo
  let inst_tp_clo = NbE.inst_tp_clo

  (** {1 Errors} *)

  module Error =
  struct
    let type_error expected actual =
      let msg = "Type Error" in
      let note = Format.asprintf "Expected '%a' = '%a'@."
          (D.pp_tp Pp.init) expected
          (D.pp_tp Pp.init) actual
      in Doctor.error ~note ~code:"E0004" msg

    let staging_mismatch expected actual =
      let msg = "Staging Mismatch" in
      let note =
        Format.asprintf "Expected staging level '%d' but got '%d'"
          expected
          actual
      in Doctor.error ~note ~code:"E0005" msg

    let staging_not_inferrable tp =
      let msg = "Inference failure" in
      let note =
        Format.asprintf "Could not infer staging level of type '%a'"
          CS.dump tp
      in Doctor.error ~note ~code:"E0006" msg

    let expected_connective conn actual =
      let msg = "Connective Mismatch" in
      let note =
        Format.asprintf "Expected a %s but got '%a'"
          conn
          (D.pp_tp Pp.init) actual
      in Doctor.error ~note ~code:"E0007" msg

    let cant_stage_zero () =
      let msg = "Staging Error" in
      let note =
        Format.asprintf "Tried to go below stage 0."
      in Doctor.error ~note ~code:"E0008" msg

    let type_not_inferrable tp =
      let msg = "Inference Error" in
      let note =
        Format.asprintf "Could not infer type of '%a'"
          CS.dump tp
      in Doctor.error ~note ~code:"E0009" msg
  end

  (** {1 Type Holes} *)
  let hole ~stage nm tp =
    let tp = quote_tp tp in
    let env = Eff.read () in
    if env.strict_mode then
      let msg = "Encountered Hole in Strict Mode." in
      let note =
        Format.asprintf "Encountered a hole of type '%a'"
          S.dump_tp tp
      in Doctor.error ~note ~code:"E0010" msg
    else if stage <> 0 then
      let msg = "Encountered Hole in a metaprogram." in
      let note =
        Format.asprintf "Encountered a hole of type '%a' while in stage %d"
          S.dump_tp tp
          stage
      in Doctor.error ~note ~code:"E0011" msg
    else
      let msg = "Type Hole" in
      let note =
        Format.asprintf "Encountered a hole of type '%a'"
          S.dump_tp tp
      in
      Doctor.warning ~note ~code:"W0002" msg;
      S.Hole nm

  let check_stage ~expected ~actual =
    if expected = actual then
      ()
    else
      Error.staging_mismatch expected actual

  (** {1 Elaborating Types} *)

  let rec check_tp (tm : CS.t) ~(stage : int) : S.tp =
    Doctor.locate tm.info @@ fun () ->
    match tm.node with
    | CS.Pi (base, name, fam) ->
      let base = check_tp ~stage base in
      let vbase = eval_tp ~stage base in
      let fam = bind_var name stage vbase @@ fun _ ->
        check_tp fam ~stage
      in S.Pi (base, name, fam)
    | _ ->
      let (tp, inferred_stage) = infer_tp tm in
      if stage = inferred_stage then
        tp
      else
        Error.staging_mismatch stage inferred_stage

  and infer_tp (tm : CS.t) : (S.tp * int) =
    Doctor.locate tm.info @@ fun () ->
    match tm.node with
    | CS.Pi (base, ident, fam) ->
      let base, base_stage = infer_tp base in
      let vbase = eval_tp ~stage:base_stage base in
      let fam = bind_var ident base_stage vbase @@ fun _ ->
        check_tp fam ~stage:base_stage
      in S.Pi (base, ident, fam), base_stage
    | CS.Expr tp ->
      let (tp, stage) = infer_tp tp in
      S.Expr tp, stage + 1
    | CS.Univ {stage} ->
      S.Univ stage, stage
    | _ ->
      Error.staging_not_inferrable tm

  and check_code_pi ~stage base x fam =
    let base = check base ~stage (D.Univ stage) in
    let base_tp = NbE.do_el ~stage @@ eval ~stage base in
    let fam = bind_var x stage base_tp @@ fun _ ->
      check fam ~stage (D.Univ stage)
    in
    S.CodePi (base, S.Lam (x, fam))

  and check_code_expr ~stage tm =
    if stage <> 0 then
      let tm = check ~stage:(stage - 1) tm (D.Univ (stage - 1)) in
      S.CodeExpr tm
    else
      Error.cant_stage_zero ()

  and check_args stage tp tms =
    match tp, tms with
    | tp, [] -> [], tp
    | (D.Pi (base, _, fam)), (tm :: tms) ->
      let tm = check tm ~stage base in
      let vtm = eval ~stage tm in
      let fib = inst_tp_clo ~stage fam vtm in
      let tms, ret = check_args stage fib tms in
      (tm :: tms, ret)
    | _ ->
      Error.expected_connective "a pi type" tp

  (** {1 Elaborating Terms} *)
  and check (tm : CS.t) ~(stage : int) (tp : D.tp) : S.t =
    Doctor.locate tm.info @@ fun () ->
    match tm.node, tp with
    | CS.Lam (names, body), D.Pi (base, _, fam) ->
      let rec check_lams names tp =
        match names with
        | [] -> check ~stage body tp
        | name :: names ->
          bind_var name stage base @@ fun arg ->
          let fib = inst_tp_clo ~stage fam arg in
          let body = check_lams names fib in
          S.Lam(name, body)
      in
      check_lams names tp
    | CS.Pi (base, x, fam), D.Univ _ ->
      check_code_pi ~stage base x fam
    | CS.Expr tm, D.Univ _ ->
      check_code_expr ~stage tm 
    | CS.Quote tm, D.Expr tp ->
      if stage > 0 then
        S.Quote (check tm ~stage:(stage - 1) tp)
      else
        Error.cant_stage_zero ()
    | CS.Hole nm, tp ->
      hole ~stage nm tp
    | _ ->
      let tm', _, tp' = infer tm in
      try NbE.equate_tp ~size:0 tp tp'; tm' with
      | NbE.NotConvertible ->
        Error.type_error tp tp'

  and infer_with_stage ~(stage : int) (tm : CS.t) : S.t * D.tp = 
    Doctor.locate tm.info @@ fun () ->
    match tm.node with
    | CS.Ann {tm;tp} ->
      let tp = check_tp ~stage tp in
      let vtp = eval_tp ~stage tp in
      check ~stage tm vtp, vtp
    | CS.Var nm ->
      begin
        match resolve_local nm with
        | Some ix ->
          let cell = get_local ix in
          check_stage ~expected:stage ~actual:cell.stage;
          S.Local ix, cell.tp
        | None ->
          let (gbl, stage, tp) = resolve_global nm in
          check_stage ~expected:stage ~actual:stage;
          S.Global gbl, tp
      end
    | CS.Hole _ ->
      Error.type_not_inferrable tm
    | CS.Pi (base, x, fam) ->
      let tm = check_code_pi ~stage base x fam in
      tm, D.Univ stage
    | CS.Lam (_, _) ->
      Error.type_not_inferrable tm
    | CS.Ap (t, ts) ->
      let f_tm, f_tp = infer_with_stage ~stage t in
      let tms, tp = check_args stage f_tp ts in
      S.apps f_tm tms, tp
    | CS.Expr t ->
      check_code_expr ~stage t, D.Univ stage
    | CS.Quote t ->
      if stage <> 0 then
        let tm, tp = infer_with_stage ~stage:(stage - 1) t in
        S.Quote tm, D.Expr tp
      else
        Error.cant_stage_zero ()
    | CS.Splice t ->
      let tm, tp = infer_with_stage ~stage:(stage + 1) t in
      begin
        match tp with
        | D.Expr tp ->
          S.Splice tm, tp
        | _ ->
          Error.expected_connective "an expression type" tp
      end
    | CS.Univ {stage = ustage} ->
      if stage = ustage then
        S.CodeUniv stage, D.Univ stage
      else
        Error.staging_mismatch stage ustage


  and infer (tm : CS.t) : S.t * int * D.tp =
    Doctor.locate tm.info @@ fun () ->
    match tm.node with
    | CS.Var nm ->
      begin
        match resolve_local nm with
        | Some ix ->
          let cell = get_local ix in
          S.Local ix, cell.stage, cell.tp
        | None ->
          let (gbl, stage, tp) = resolve_global nm in
          S.Global gbl, stage, tp
      end
    | CS.Ap (t, ts) ->
      let f_tm, f_stage, f_tp = infer t in
      let tms, tp = check_args f_stage f_tp ts in
      S.apps f_tm tms, f_stage, tp
    | CS.Splice tm ->
      let tm, stage, tp = infer tm in
      begin
        match tp with
        | D.Expr tp ->
          S.Splice tm, stage - 1, tp
        | _ ->
          Error.expected_connective "an expresssion type" tp
      end
    (* [TODO: Reed M, 03/05/2022] is this right? *)
    | CS.Univ {stage} ->
      S.CodeUniv stage, stage, D.Univ stage
    | CS.Ann {tm; tp} ->
      let (tp, stage) = infer_tp tp in
      let vtp = eval_tp ~stage tp in
      let tm = check tm ~stage vtp in
      (tm, stage, vtp)
    | _ ->
      Error.type_not_inferrable tm
end

let check_tp tp ~stage =
  let env = create_env 128 in
  Eff.run ~env @@ fun () -> check_tp tp ~stage

let infer_tp tp =
  let env = create_env 128 in
  Eff.run ~env @@ fun () -> infer_tp tp

let check tm ~stage tp =
  let env = create_env 128 in
  Eff.run ~env @@ fun () -> check tm ~stage tp

let infer_with_stage ~stage tm =
  let env = create_env 128 in
  Eff.run ~env @@ fun () -> infer_with_stage ~stage tm

let infer tm =
  let env = create_env 128 in
  Eff.run ~env @@ fun () -> infer tm

