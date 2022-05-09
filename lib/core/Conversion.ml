module S = Syntax
module D = Domain

exception NotConvertible

open struct

  type mode =
    | Full
    (** When in {!const.Full} mode, the conversion checker will unfold
        {i all} {!const.Syntax.Global} values before checking convertability. *)
    | Rigid
    (** When in {!const.Rigid} mode, we will only unfold {!const.Syntax.Global}
        values if their heads don't match. *)
    | Flex
    (** When in {!const.Flex} mode, we don't unfold {!const.Syntax.Globals}
        at all *)

  type env =
    { mode : mode;
      size : int }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let get_mode () =
    (Eff.read()).mode

  let with_mode s f =
    Eff.scope (fun env -> {env with mode = s}) f

  (* [TODO: Reed M, 28/04/2022] Can we use Lazy.force_val here? *)
  let unfold (tm : D.t) : D.t =
    match get_mode() with
    | Full -> Eval.unfold tm
    | _ -> tm

  let bind_var (f : D.t -> 'a) : 'a =
    let arg = D.local (Eff.read()).size in
    Eff.scope (fun env -> {env with size = env.size + 1}) @@ fun () ->
    f arg

  (*******************************************************************************
   * Equating Values *)

  let rec equate (v0 : D.t) (v1 : D.t) : unit =
    let mode = get_mode() in
    match unfold v0, unfold v1 with
    (* Straightforward syntactic checks *)
    | D.Neu neu0, D.Neu neu1 ->
      equate_neu neu0 neu1
    | D.Lam (_, clo0), D.Lam (_, clo1) ->
      equate_tm_clo clo0 clo1
    | D.Quote v0, D.Quote v1 ->
      equate v0 v1
    | D.Code code0, D.Code code1 ->
      equate_code code0 code1
    (* We unfold globals when in Rigid mode before attempting eta-expansion *)
    | D.Neu { hd = D.Global(_, unf); _ }, v
    | v, D.Neu { hd = D.Global(_, unf); _ } when mode = Rigid ->
      equate (Lazy.force unf) v
    (* When we have a neutral, we need to attempt to eta-expand. *)
    | D.Neu neu, tm
    | tm, D.Neu neu ->
      equate_eta neu tm
    | _ -> raise NotConvertible

  and equate_field (lbl0, v0) (lbl1, v1) =
    if lbl0 = lbl1 then
      equate v0 v1
    else 
      raise NotConvertible

  (*******************************************************************************
   * Equating Types *)

  and equate_tp (tp0 : D.tp) (tp1 : D.tp) : unit =
    match tp0, tp1 with
    | D.Pi (base0, _, fam0), D.Pi (base1, _, fam1) ->
      equate_tp base0 base1;
      equate_tp_clo fam0 fam1
    | D.Univ stage0, D.Univ stage1 when stage0 = stage1 ->
      ()
    | D.Expr tp0, D.Expr tp1 ->
      equate_tp tp0 tp1
    | D.El code0, D.El code1 ->
      equate_code code0 code1
    | D.ElNeu neu0, D.ElNeu neu1 ->
      equate_neu neu0 neu1
    | tp0, tp1 -> equate_eta_tp tp0 tp1

  (*******************************************************************************
   * Equating Codes *)

  and equate_code (code0 : D.code) (code1 : D.code) : unit =
    match code0, code1 with
    | CodePi (base0, fam0), CodePi (base1, fam1) ->
      equate base0 base1;
      equate fam0 fam1
    | CodeUniv stage0, CodeUniv stage1 when stage0 = stage1 -> 
      ()
    | _ -> raise NotConvertible

  (*******************************************************************************
   * Equating Neutrals *)

  and equate_neu (neu0 : D.neu) (neu1 : D.neu) : unit =
    match neu0.hd, neu1.hd with
    | D.Local ix0, D.Local ix1 when ix0 = ix1 -> ()
    | D.Global (nm0, v0), D.Global (nm1, v1) ->
      equate_globals nm0 nm1 v0 v1 neu0.spine neu1.spine
    | _ -> raise NotConvertible

  and equate_globals nm0 nm1 v0 v1 spine0 spine1 : unit =
    match get_mode() with
    | Flex ->
      (* Flex Mode: perform any unfolding at all. *)
      if nm0 = nm1 then
        equate_spine spine0 spine1
      else
        raise NotConvertible
    | Rigid ->
      (* Rigid Mode: If the heads match, try to equate the spines without performing
         any unfolding. If this fails, we switch into full mode to avoid any further
         backtracking. *)
      if nm0 = nm1 then
        try
          with_mode Flex @@ fun () -> equate_spine spine0 spine1
        with NotConvertible ->
          with_mode Full @@ fun () -> equate (Lazy.force v0) (Lazy.force v1)
      else
        equate (Lazy.force v0) (Lazy.force v1)
    | Full ->
      failwith "The impossible happened! We didn't unfold a global when in Full mode."

  and equate_spine (spine0 : D.frm list) (spine1 : D.frm list) : unit =
    match spine0, spine1 with
    | [], [] -> ()
    | (frm0 :: spine0, frm1 :: spine1) ->
      equate_frm frm0 frm1;
      equate_spine spine0 spine1
    | _ -> raise NotConvertible

  and equate_frm (frm0 : D.frm) (frm1 : D.frm) : unit =
    match frm0, frm1 with
    | D.Ap v0, D.Ap v1 ->
      equate v0 v1
    | D.Splice, D.Splice ->
      ()
    | _ -> raise NotConvertible

  (** {1 Equating with Eta Expansion} *)

  and equate_eta (neu0 : D.neu) (v1 : D.t) =
    match unfold v1 with
    | D.Neu neu1 ->
      if neu0.hd = neu1.hd then
        equate_spine neu0.spine neu1.spine
      else
        raise NotConvertible
    | D.Lam (_, clo) ->
      bind_var @@ fun arg ->
      equate_eta (D.push_frm neu0 (D.Ap arg) ~unfold:(fun v -> Eval.do_ap v arg)) (Eval.inst_tm_clo clo arg)
    | D.Quote v1 ->
      equate_eta (D.push_frm neu0 D.Splice ~unfold:Eval.do_splice) v1
    | _ -> raise NotConvertible

  and equate_eta_tp (tp0 : D.tp) (tp1 : D.tp) =
    match tp0, tp1 with
    (* Because we aren't using weak tarski universes, we need to unfold
       layers of El here. *)
    | D.El code, tp
    | tp, D.El code ->
      equate_tp tp (Eval.unfold_el code)
    (* [TODO: Reed M, 29/04/2022] What happens when we try to equate an ElNeu with a type? *)
    | _ -> raise NotConvertible


  (** {1 Equating Closures} *)

  and equate_tm_clo clo0 clo1 =
    bind_var @@ fun arg ->
    equate (Eval.inst_tm_clo clo0 arg) (Eval.inst_tm_clo clo1 arg)

  and equate_tp_clo clo0 clo1 =
    bind_var @@ fun arg ->
    equate_tp (Eval.inst_tp_clo clo0 arg) (Eval.inst_tp_clo clo1 arg)
end

(*******************************************************************************
 * Public Interface *)

let equate ~size v0 v1 =
  Eff.run ~env:{mode = Rigid; size} @@ fun () -> equate v0 v1

let equate_tp ~size tp0 tp1 =
  Eff.run ~env:{mode = Rigid; size} @@ fun () -> equate_tp tp0 tp1
