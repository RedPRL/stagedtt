open Prelude
open Core
open Eff

open Command
open Elaborator


module CS = Elaborator.Syntax
module S = Core.Syntax
module D = Core.Domain

type status =
  | Continue
  | Quit

let print_diagnostic path tm stage tp =
  let message =
    Format.asprintf "@[<hov>%a@ : %a@ :[%d]= %a@]"
      Ident.pp_path path
      (S.pp_tp Pp.init) tp
      stage
      (S.pp Pp.init) tm
  in Diagnostic.info ~code:"I0001" message

let nf_diagnostic tm nf =
  let message =
    Format.asprintf "@[%a@ ~> %a@]"
      S.dump tm
      S.dump nf
  in Diagnostic.info ~code:"I0002" message

let stage_diagnostic path st =
  let message =
    Format.asprintf "@[%a@ ~> %a@]"
      Ident.pp_path path
      S.dump st
  in Diagnostic.info ~code:"I0003" message

let not_implemented string =
  let message = Format.asprintf "%s is not implemented yet!" string in
  let diag = Diagnostic.warning ~code:"W0001" message in
  Diagnostic.pp Format.err_formatter diag;
  Continue

let expected_staged () =
  let message = Format.asprintf "The 'def!' construct requires the term to be of stage > 0, but it wasn't." in
  let diag = Diagnostic.warning ~code:"E0012" message in
  Diagnostic.pp Format.err_formatter diag

let elab tp tm =
  match tp with
  | Some tp ->
    Debug.print ~file:"Execute.ml" "Elaborating type %a@." CS.dump tp;
    let tp = Refiner.check_tp ~stage:0 tp in
    Debug.print ~file:"Execute.ml" "Evaluating type %a@." S.dump_tp tp;
    let vtp = NbE.eval_tp ~stage:0 ~env:D.Env.empty tp in
    Debug.print  ~file:"Execute.ml" "Checking term %a@." CS.dump tm;
    let tm = Refiner.check tm ~stage:0 vtp in
    (tm, vtp)
  | None ->
    Refiner.infer_with_stage ~stage:0 tm

let elab_staged tp tm =
  match tp with
  | Some tp ->
    let (tp, stage) = Refiner.infer_tp tp in
    Debug.print ~file:"Execute.ml" "Evaluating type %a@." S.dump_tp tp;
    let vtp = NbE.eval_tp ~stage ~env:D.Env.empty tp in
    Debug.print  ~file:"Execute.ml" "Checking term %a@." CS.dump tm;
    let tm = Refiner.check tm ~stage vtp in
    (tm, stage, vtp)
  | None ->
    Refiner.infer tm

let resolve_value path =
  let (gbl, stage, vtp) = Namespace.resolve path in
  match gbl with
  | `Unstaged (_, v, _) -> v, stage, vtp
  | `Staged (_, v, _, _) -> v, stage, vtp

let resolve_inner path =
  let (gbl, stage, vtp) = Namespace.resolve path in
  match gbl with
  | `Unstaged (_, _, iv) -> iv, stage, vtp
  | `Staged (_, _, iv, _) -> iv, stage, vtp

let exec_command : command -> status =
  function
  | Def {ident = User path; tp; tm} ->
    let (tm, vtp) = elab tp tm in
    let vtm = lazy (NbE.eval ~stage:0 ~env:D.Env.empty tm) in
    let itm = lazy (Stage.eval_inner ~stage:0 tm) in
    Namespace.define path (`Unstaged (path, vtm, itm)) 0 vtp;
    Continue
  | Def {ident = Anon; tp; tm} ->
    let _ = elab tp tm in
    Continue
  | DefStaged { ident = User path; tp; tm } ->
    let (tm, stage, vtp) = elab_staged tp tm in
    let vtm = lazy (NbE.eval ~stage ~env:D.Env.empty tm) in
    let itm = lazy (Stage.eval_inner ~stage tm) in
    let otm = Stage.eval_outer ~stage tm in
    if stage = 0 then
      expected_staged ()
    else
      Namespace.define path (`Staged (path, vtm, itm, otm)) 0 vtp;
    Continue
  | DefStaged { ident = Anon; tp; tm } ->
    let (_, stage, _) = elab_staged tp tm in
    if stage = 0 then
      expected_staged ();
    Continue
  | Fail {message; tp; tm} ->
    begin
      try
        let _ = elab tp tm in
        Diagnostic.pp Format.err_formatter @@
        Diagnostic.error ~code:"E0003" @@
        Format.asprintf "Expected failure '%s'." message
      with
      | [%effect? Doctor.Fatal diag, _] ->
        Diagnostic.pp Format.std_formatter @@
        Diagnostic.info ~code:"I0003" @@
        Format.asprintf "Encountered expected failure@.@[<v 2>%a@]"
          Diagnostic.pp diag
      | exn ->
        Diagnostic.pp Format.std_formatter @@
        Diagnostic.info ~code:"I0003" @@
        Format.asprintf "Encountered expected exception@.@[<v 2>%s@]"
          (Printexc.to_string exn)
    end;
    Continue
  | Normalize { tm } ->
    let (tm, stage, _) = Refiner.infer tm in
    let vtm = Unfold.unfold_top ~stage @@ NbE.eval ~stage ~env:D.Env.empty tm in
    let nf = NbE.quote ~size:0 vtm in
    Diagnostic.pp Format.std_formatter @@
    nf_diagnostic tm nf;
    Continue
  | Stage path ->
    let (iv, _, _) = resolve_inner path in
    let iv = Unfold.unfold_inner @@ Lazy.force iv in
    let tm = Stage.quote_inner ~size:0 iv in
    Diagnostic.pp Format.std_formatter @@
    stage_diagnostic path tm;
    Continue
  | Print path ->
    let (vtm, stage, vtp) = resolve_value path in
    let tp = NbE.quote_tp ~size:0 vtp in
    let tm = NbE.quote ~size:0 (Lazy.force vtm) in
    Diagnostic.pp Format.std_formatter @@
    print_diagnostic path tm stage tp;
    Continue
  | Debug mode ->
    Debug.debug_mode mode;
    Continue
  | Quit -> Quit

let rec exec : command list -> unit =
  function
  | [] -> ()
  | cmd :: cmds ->
    begin
      match exec_command cmd with
      | Continue -> exec cmds
      | Quit -> ()
    end

let load input =
  match Loader.load input with
  | Ok (cmds, lexbuf) ->
    begin
      try
        Doctor.run lexbuf @@ fun () ->
        Namespace.run @@ fun () ->
        exec cmds
      with
      | [%effect? Doctor.Fatal diag, _] ->
        Diagnostic.pp Format.err_formatter diag;
      | [%effect? Doctor.Survivable diag, k] ->
        Diagnostic.pp Format.std_formatter diag;
        Effect.Deep.continue k ()
    end
  | Error diagnostic -> Diagnostic.pp Format.err_formatter diagnostic
