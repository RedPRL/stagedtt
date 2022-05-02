open Bwd
module Bwd = BwdLabels

module CS = Syntax

module S = Core.Syntax
module D = Core.Domain

(** {1 Effects} *)

type cell =
  { name  : string;
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

open struct
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
    BwdLabels.nth (get_locals ()) idx

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

  let eval = lift_eval Core.Eval.eval
  let eval_tp = lift_eval Core.Eval.eval_tp
  let quote = lift_quote Core.Quote.quote
  let quote_tp = lift_quote Core.Quote.quote_tp

  let inst_tm_clo = Core.Eval.inst_tm_clo
  let inst_tp_clo = Core.Eval.inst_tp_clo

  (** {1 Errors} *)

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
    | CS.Sign sign ->
      let sign = List.map (fun (lbl, field_tp) -> (lbl, check_tp field_tp ~stage)) sign in
      S.Sign sign
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
    | CS.Sign sign ->
      (* [FIXME: Reed M, 29/04/2022] Right now we assume that all the types of a signature live at
         stage 0 so we can experiment more readily *)
      let sign = List.map (fun (lbl, field) -> (lbl, check_tp field ~stage:0)) sign in
      S.Sign sign, 0
    | CS.Univ {stage} ->
      S.Univ stage, stage
    | _ -> raise @@ TypeError "Type annotation required"
end

let check_tp ~env tp ~stage =
  Eff.run ~env @@ fun () -> check_tp tp ~stage

let infer_tp ~env tp =
  Eff.run ~env @@ fun () -> infer_tp tp
