open Prelude

module CS = Syntax

open Core

module S = Core.Syntax
module D = Core.Domain

(** {1 Effects} *)
type _ Effect.t +=
  | Resolve : Ident.t -> (D.t Lazy.t * D.tp) Effect.t
  | Diagnostic : string -> unit Effect.t

let resolve_global nm =
  Effect.perform (Resolve nm)

open struct
  type cell =
    { name  : Ident.t;
      tp    : D.tp;
      value : D.t Lazy.t }

  type env =
    { locals : cell bwd;
      (* [NOTE: Caching Env Sizes]
         We use a (reversed) linked list to store our local environments.
         This has good implications for sharing, but comes with a fatal flaw:
         computing the length is an O(n) operation, and it's something we do
         a /lot/. Therefore, we cache the length of the env to avoid recomputing
         it.

         Invariant: Bwd.length locals = size *)
      size   : int }

  let empty_env = { locals = Emp; size = 0 }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let get_locals () =
    (Eff.read ()).locals

  let get_size () =
    (Eff.read ()).size

  (** {2 Variable Resolution} *)

  let resolve_local ident =
    let exception E in
    let rec find_idx idx =
      function
      | Emp -> raise E
      | Snoc (xs, cell) ->
        if cell.name = ident then
          idx
        else find_idx (idx + 1) xs
    in try
      Some (find_idx 0 (get_locals()))
    with E -> None

  let get_local idx =
    Bwd.nth (get_locals ()) idx

  (** {2 Variable Binding} *)

  let bind_var name tp k =
    let push_var env = 
      let value = Lazy.from_val @@ D.local (get_size ()) in
      { locals = Snoc(env.locals, { name; tp; value }); size = env.size + 1 }
    in
    Eff.scope push_var @@ fun () ->
    k (get_local 0)

  (** {1 Wrappers for NbE} *)

  let lift_eval (f : env:D.env -> 'a) : 'a =
    let locals = get_locals () in
    (* [FIXME: Reed M, 28/04/2022] This is bad! We should find a way to share
       datastructures here... *)
    let values = Bwd.map ~f:(fun cell -> cell.value) locals in
    let size = get_size () in
    f ~env:(D.Env.from_vals values size)

  let lift_quote (f : size:int -> 'a) : 'a =
    f ~size:(get_size())

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
  Eff.run ~env:empty_env @@ fun () -> check_tp tp ~stage

let infer_tp tp =
  Eff.run ~env:empty_env @@ fun () -> infer_tp tp

let check tm tp =
  Eff.run ~env:empty_env @@ fun () -> check tm tp

let infer tm =
  Eff.run ~env:empty_env @@ fun () -> infer tm
