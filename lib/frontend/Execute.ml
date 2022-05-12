open Prelude
open Command
open Elaborator
open Core


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

let elab tp tm =
  match tp with
  | Some tp ->
    Debug.print "Elaborating type %a@." CS.dump tp;
    let (tp, stage) = Refiner.infer_tp tp in
    Debug.print "Evaluating type %a@." S.dump_tp tp;
    let vtp = NbE.eval_tp ~stage ~env:D.Env.empty tp in
    Debug.print "Checking term %a@." CS.dump tm;
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
  | Declare {ident = User path; tp; tm} ->
    let (tm, tm_stage, vtp) = elab tp tm in
    let vtm =
      lazy begin
        NbE.eval ~stage:tm_stage ~env:D.Env.empty tm
      end in
    let itm =
      lazy begin
        Stage.eval_inner ~tm_stage tm
      end
    in
    let expand =
      Stage.eval_outer ~tm_stage tm
    in
    let gbl : S.global =
      if tm_stage = 0 then
        `Unstaged (path, vtm, itm)
      else
        `Staged (path, vtm, itm, expand)
    in
    Namespace.define path gbl tm_stage vtp;
    Continue
  | Declare {ident = Anon; tp; tm} ->
    let (_, _, _) = elab tp tm in
    Continue
  | Fail {message; tp; tm} ->
    begin
      try
        let (_, _, _) = elab tp tm in
        Diagnostic.pp Format.err_formatter @@
        Diagnostic.error ~code:"E0003" @@
        Format.asprintf "Expected failure '%s'." message
      with
      | Diagnostic.Fatal diag ->
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
    let tm = Stage.quote_inner ~size:0 (Lazy.force iv) in
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
  | Ok cmds ->
    begin
      try
        Namespace.run @@ fun () ->
        exec cmds
      with Diagnostic.Fatal diag ->
        Diagnostic.pp Format.err_formatter diag
    end
  | Error diagnostic -> Diagnostic.pp Format.err_formatter diagnostic
