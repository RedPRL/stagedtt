open Prelude

type mode =
  | Inner
  | Outer

type _ Effect.t +=
  | GetStage : int Effect.t

let run ~(stage:int) k =
  let open Effect.Deep in
  try_with k ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | GetStage -> Option.some @@ fun (k : (a, _) continuation) ->
            continue k stage
          | _ -> None
    }

let get_stage () =
  Effect.perform GetStage

let incr_stage stage k =
  let current_stage = Effect.perform GetStage in
  let mode =
    if current_stage + 1 > stage then Outer else Inner
  in
  run ~stage:(current_stage + 1) @@ fun () -> k mode

let decr_stage stage k =
  let current_stage = Effect.perform GetStage in
  let mode =
    if current_stage - 1 <= stage then Inner else Outer
  in
  run ~stage:(current_stage - 1) @@ fun () -> k mode
