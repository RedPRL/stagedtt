open Core

module D = Domain
module I = Inner

open struct
  type env =
    { size : int;
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

  (* [TODO: Reed M, 12/05/2022] Actually check if we should unfold a name! *)
  let should_unfold _ =
    true

  let rec unfold v =
    match v with
    | D.Neu { hd = D.Global(nm, unf); spine } ->
      if should_unfold nm then
        unfold (Lazy.force unf)
      else D.Neu { hd = D.Global (nm, unf); spine = unfold_spine spine }
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
    | Clo (tm, env) ->
      let unf = unfold @@ NbE.eval ~env:(D.Env.extend env v) tm in
      Clo (quote unf, env)

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
end


let unfold_top v =
  let env = { size = 0; pattern = () } in
  Eff.run ~env @@ fun () ->
  unfold v

let unfold ~size v =
  let env = { size; pattern = () } in
  Eff.run ~env @@ fun () ->
  unfold v
