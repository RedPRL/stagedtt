open Yuujinchou
open Elaborator

module D = Core.Domain

type empty = |

module NS = Scope.Make(struct type data = (D.tp * D.t Lazy.t) type hook = empty end)

let define path tp tm =
  NS.include_singleton (path, (tp, tm))

let run f =
  let handle_resolve_global () =
    try f () with
    | [%effect? (Refiner.Resolve ident), k] ->
      failwith "[FIXME] Yuujinchou.empty.run: Implement this!"
  in
  let open Effect.Deep in
  try handle_resolve_global () with
  | [%effect? NS.Act.BindingNotFound _, k] ->
    continue k ()
  | [%effect? NS.Act.Shadowing {latter; _}, k] -> continue k latter
  | [%effect? NS.Act.Hook _, _] -> .
