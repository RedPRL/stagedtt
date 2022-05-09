open Command
open Elaborator
open Core

module CS = Elaborator.Syntax
module S = Core.Syntax
module D = Core.Domain

type status =
  | Continue
  | Quit

let not_implemented string =
  let message = Format.asprintf "%s is not implemented yet!" string in
  let diag = Diagnostic.warning ~code:"W0001" message in
  Diagnostic.pp Format.err_formatter diag;
  Continue

let exec_command : command -> status =
  function
  | Declare {ident; tp = Some tp; tm} ->
    let (tp, _) = Refiner.infer_tp tp in
    let vtp = Eval.eval_tp ~env:D.Env.empty tp in
    let tm = Refiner.check tm vtp in
    Continue
  | Declare {ident; tp = None; tm} ->
    not_implemented "def"
  | Fail _ ->
    not_implemented "#fail"
  | Normalize _ ->
    not_implemented "#normalize"
  | Stage _ ->
    not_implemented "#stage"
  | Print _ ->
    not_implemented "#print"
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
  | Ok cmds -> exec cmds
  | Error diagnostic -> Diagnostic.pp Format.err_formatter diagnostic
