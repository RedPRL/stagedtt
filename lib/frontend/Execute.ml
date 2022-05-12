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

let stage_diagnostic tm nf =
  let message =
    Format.asprintf "@[%a@ ~> %a@]"
      S.dump tm
      S.dump nf
  in Diagnostic.info ~code:"I0003" message

let not_implemented string =
  let message = Format.asprintf "%s is not implemented yet!" string in
  let diag = Diagnostic.warning ~code:"W0001" message in
  Diagnostic.pp Format.err_formatter diag;
  Continue

let define ident tm stage tp =
  match ident with
  | Ident.User path -> 
    Namespace.define path tm stage tp
  | Ident.Anon -> ()

let elab tp tm =
  match tp with
  | Some tp ->
    let (tp, stage) = Refiner.infer_tp tp in
    let vtp = NbE.eval_tp ~env:D.Env.empty tp in
    let tm = Refiner.check tm ~stage vtp in
    (tm, stage, vtp)
  | None ->
    Refiner.infer tm

let exec_command : command -> status =
  function
  | Declare {ident; tp; tm} ->
    let (tm, stage, vtp) = elab tp tm in
    let vtm =
      lazy begin
        NbE.eval ~env:D.Env.empty tm
      end in
    define ident vtm stage vtp;
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
    let (tm, _, _) = Refiner.infer tm in
    let vtm = Unfold.unfold_top @@ NbE.eval ~env:D.Env.empty tm in
    let nf = NbE.quote ~size:0 vtm in
    Diagnostic.pp Format.std_formatter @@
    nf_diagnostic tm nf;
    Continue
  | Stage { stage; tm } ->
    let (tm, tm_stage, _) = Refiner.infer tm in
    let stm = Stage.stage ~stage ~tm_stage tm in
    Diagnostic.pp Format.std_formatter @@
    stage_diagnostic tm stm;
    Continue
  | Print path ->
    let (vtm, stage, vtp) = Namespace.resolve path in
    let tp = NbE.quote_tp ~size:0 vtp in
    let tm = NbE.quote ~size:0 (Lazy.force vtm) in
    Diagnostic.pp Format.std_formatter @@
    print_diagnostic path tm stage tp;
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
