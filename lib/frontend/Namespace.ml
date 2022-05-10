open Algaeff.StdlibShim

open Prelude
open Yuujinchou
open Elaborator

module D = Core.Domain

type empty = |

module NS = Scope.Make(struct type data = (D.t Lazy.t * D.tp) type hook = empty end)

let define path tp tm =
  NS.include_singleton (path, (tp, tm))

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
        let msg = Format.asprintf "Identifier '%a' is not in scope." Ident.pp_path path in
        let diag = Diagnostic.error ~code:"E0001" msg
        in discontinue k @@ Diagnostic.Fatal diag
  in
  try handle_resolve_global () with
  | [%effect? NS.Act.BindingNotFound _, k] -> continue k ()
  | [%effect? NS.Act.Shadowing {latter; _}, k] -> continue k latter
  | [%effect? NS.Act.Hook _, _] -> .
