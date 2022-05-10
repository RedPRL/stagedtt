open Prelude

module Ctx = OrderedHashTbl.Make(Ident)

module CS = Syntax

open Core

module S = Core.Syntax
module D = Core.Domain

(** {1 Effects} *)
type _ Effect.t +=
  | Resolve : Yuujinchou.Trie.path -> (D.t Lazy.t * D.tp) Effect.t

let resolve_global nm =
  Effect.perform (Resolve nm)

open struct
  type cell = { tp : D.tp; value : D.t Lazy.t }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = cell Ctx.t end)

  (** {2 Variable Resolution} *)

  let resolve_local ident =
    Ctx.find_idx_of (User ident) (Eff.read ())

  let get_local idx =
    snd @@ Ctx.nth idx (Eff.read ())

  (** {2 Variable Binding} *)

  let bind_var name tp k =
    let ctx = Eff.read () in
    let value = Lazy.from_val @@ D.local (Ctx.size ctx) in
    Ctx.scope name { tp; value } ctx @@ fun () ->
    k (snd @@ Ctx.peek ctx)

  (** {1 Wrappers for NbE} *)

  let lift_eval (f : env:D.env -> 'a) : 'a =
    let ctx = Eff.read () in
    (* [FIXME: Reed M, 28/04/2022] This is bad! We should find a way to share
       datastructures here... *)
    let values = Ctx.values_with ctx (fun cell -> cell.value) in
    let size = Ctx.size ctx in
    f ~env:(D.Env.from_vals values size)

  let lift_quote (f : size:int -> 'a) : 'a =
    let ctx = Eff.read () in
    f ~size:(Ctx.size ctx)

  let eval tm = lift_eval Core.Eval.eval tm
  let eval_tp tp = lift_eval Core.Eval.eval_tp tp
  let quote tm = lift_quote Core.Quote.quote tm
  let quote_tp tp = lift_quote Core.Quote.quote_tp tp

  let inst_tm_clo = Core.Eval.inst_tm_clo
  let inst_tp_clo = Core.Eval.inst_tp_clo

  (** {1 Errors} *)

  (* [TODO: Reed M, 02/05/2022] Think these through more... *)
  exception TypeError of string

  (** {1 Elaborating Types} *)

  let rec check_tp (tm : CS.t) ~(stage:int) : S.tp =
    match tm with
    | CS.Pi (base, name, fam) ->
      let base = check_tp base ~stage in
      let vbase = eval_tp base in
      let fam = bind_var name vbase @@ fun _ ->
        check_tp fam ~stage
      in S.Pi (base, name, fam)
    | tm ->
      let (tp, inferred_stage) = infer_tp tm in
      if stage = inferred_stage then
        tp
      else
        raise @@ TypeError "Stage mismatch"

  and infer_tp (tm : CS.t) : (S.tp * int) =
    match tm with
    | CS.Pi (base, ident, fam) ->
      let base, base_stage = infer_tp base in
      let vbase = eval_tp base in
      let fam = bind_var ident vbase @@ fun _ ->
        check_tp fam ~stage:base_stage
      in S.Pi (base, ident, fam), base_stage
    | CS.Expr tp ->
      let (tp, stage) = infer_tp tp in
      S.Expr tp, stage + 1
    | CS.Univ {stage} ->
      S.Univ stage, stage
    | _ -> raise @@ TypeError "Type not inferrable"

  (** {1 Elaborating Terms} *)
  and check (tm : CS.t) (tp : D.tp) : S.t =
    match tm, tp with
    | CS.Lam ([], body), tp ->
      check body tp
    | CS.Lam (name :: names, body), D.Pi (base, _, fam) ->
      bind_var name base @@ fun arg ->
      let fib = inst_tp_clo fam (Lazy.force arg.value) in
      S.Lam(name, check (CS.Lam (names, body)) fib)
    | CS.Quote tm, D.Expr tp ->
      S.Quote (check tm tp)
    | _ ->
      let tm', tp' = infer tm in
      try Conversion.equate_tp ~size:0 tp tp'; tm' with
      | Conversion.NotConvertible -> raise @@ TypeError "Not of expected type"

  (* [TODO: Reed M, 02/05/2022] Should I return the stage here? *)
  and infer (tm : CS.t) : S.t * D.tp =
    match tm with
    | CS.Var nm ->
      begin
        match resolve_local nm with
        | Some ix ->
          let cell = get_local ix in
          S.Local ix, cell.tp
        | None ->
          let (v, tp) = resolve_global nm in
          S.Global (nm, v), tp
      end
    | CS.Ap (t, ts) ->
      let rec check_args tp tms =
        match tp, tms with
        | tp, [] -> [], tp
        | (D.Pi (base, _, fam)), (tm :: tms) ->
          let tm = check tm base in
          let vtm = eval tm in
          let fib = inst_tp_clo fam vtm in
          let tms, ret = check_args fib tms in
          (tm :: tms, ret)
        | _ -> raise @@ TypeError "Expected a pi type"
      in
      let f_tm, f_tp = infer t in
      let tms, tp = check_args f_tp ts in
      S.apps f_tm tms, tp
    | CS.Splice tm ->
      let tm, tp = infer tm in
      begin
        match tp with
        | D.Expr tp ->
          S.Splice tm, tp
        | _ -> raise @@ TypeError "Expected an expression type"
      end
    (* [TODO: Reed M, 03/05/2022] is this right? *)
    | CS.Univ {stage} ->
      S.CodeUniv stage, D.Univ stage
    | CS.Ann {tm; tp} ->
      let (tp, _) = infer_tp tp in
      let vtp = eval_tp tp in
      let tm = check tm vtp in
      (tm, vtp)
    | _ -> raise @@ TypeError (Format.asprintf "Not inferrable: %a" CS.dump tm)
end

let check_tp tp ~stage =
  let env = Ctx.create 128 in
  Eff.run ~env @@ fun () -> check_tp tp ~stage

let infer_tp tp =
  let env = Ctx.create 128 in
  Eff.run ~env @@ fun () -> infer_tp tp

let check tm tp =
  let env = Ctx.create 128 in
  Eff.run ~env @@ fun () -> check tm tp

let infer tm =
  let env = Ctx.create 128 in
  Eff.run ~env @@ fun () -> infer tm
