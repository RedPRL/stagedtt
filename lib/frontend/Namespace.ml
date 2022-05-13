open Algaeff.StdlibShim
open Yuujinchou

open Prelude
open Eff
open Elaborator

module S = Core.Syntax
module D = Core.Domain

type empty = |

module NS = Scope.Make(struct type data = (S.global * int * D.tp) type hook = empty end)

let define path gbl stage tp =
  NS.include_singleton (path, (gbl, stage, tp))

let resolve path =
  Effect.perform (Refiner.Resolve path)

let run f =
  let open Effect.Deep in
  let handle_resolve_global () =
    NS.run ~prefix:Emp @@ fun () ->
    try f () with
    | [%effect? (Refiner.Resolve path), k] ->
      match NS.resolve path with
      | Some data -> continue k data
      | None ->
        let msg = "Scope Error" in 
        let note = Format.asprintf "Identifier '%a' is not in scope." Ident.pp_path path in
        Doctor.error ~note ~code:"E0001" msg
  in
  try handle_resolve_global () with
  | [%effect? NS.Act.BindingNotFound _, k] -> continue k ()
  | [%effect? NS.Act.Shadowing {latter; _}, k] -> continue k latter
  | [%effect? NS.Act.Hook _, _] -> .
