open Core

module D = Domain
module I = Inner
module O = Outer

open struct
  type env =
    { size : int;
      stage : int;
      (* [TODO: Reed M, 12/05/2022] This is waiting on the Matcher module of
         Yuujinchou. *)
      pattern : unit }

  module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

  let quote tm =
    let size = (Eff.read ()).size in
    NbE.quote ~size tm

  let bind_var (f : D.t -> 'a) : 'a =
    Eff.scope (fun env -> { env with size = env.size + 1}) @@ fun () ->
    let size = (Eff.read ()).size in 
    f (D.local (size - 1))

  let eval ~env tm =
    let stage = (Eff.read ()).stage in
    NbE.eval ~stage ~env tm

  (* [TODO: Reed M, 12/05/2022] Actually check if we should unfold a name! *)
  let should_unfold _ =
    true

  let rec unfold v =
    match v with
    | D.Neu { hd = D.Global(`Unstaged (nm, unf, syn)); spine } ->
      if should_unfold nm then
        unfold (Lazy.force unf)
      else D.Neu { hd = D.Global (`Unstaged (nm, unf, syn)); spine = unfold_spine spine }
    | D.Neu { hd; spine } ->
      D.Neu { hd; spine = unfold_spine spine }
    | D.Lam (x, clo) ->
      bind_var @@ fun arg ->
      D.Lam(x, unfold_clo clo arg)
    | D.Quote tm ->
      D.Quote (unfold tm)
    | D.Code code ->
      D.Code (unfold_code code)

  and unfold_clo clo v =
    match clo with
    | D.Clo (tm, env) ->
      let unf = unfold @@ eval ~env:(D.Env.extend env v) tm in
      D.Clo (quote unf, env)

  and unfold_code code =
    match code with
    | D.CodePi (base, fam) ->
      D.CodePi (unfold base, unfold fam)
    | D.CodeUniv i ->
      D.CodeUniv i

  and unfold_spine spine =
    List.map unfold_frm spine

  and unfold_frm frm =
    match frm with
    | D.Ap v -> D.Ap (unfold v)
    | D.Splice -> D.Splice

  and unfold_inner iv =
    match iv with
    | I.Local ix ->
      I.Local ix
    | I.Global gbl ->
      begin
        match gbl with
        | `Unstaged (_, _, syn) -> Lazy.force syn
        | `Staged (_, _, syn, _) -> Lazy.force syn
      end
    | I.Lam (x, body) ->
      I.Lam (x, unfold_inner body)
    | I.Ap (fn, arg) -> 
      I.Ap (unfold_inner fn, unfold_inner arg)
    | I.Quote tm ->
      I.Quote (unfold_inner tm)
    | I.Splice tm ->
      I.Splice (unfold_inner tm)
    | I.CodePi (base, fam) ->
      I.CodePi (unfold_inner base, unfold_inner fam)
    | I.CodeUniv stage ->
      I.CodeUniv stage
end


let unfold_top ~stage v =
  let env = { stage; size = 0; pattern = () } in
  Eff.run ~env @@ fun () ->
  unfold v

let unfold ~stage ~size v =
  let env = { stage; size; pattern = () } in
  Eff.run ~env @@ fun () ->
  unfold v

let unfold_inner iv =
  unfold_inner iv
