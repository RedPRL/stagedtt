type _ Effect.t +=
  | GetSize : int Effect.t

let run ~(size:int) k =
  let open Effect.Deep in
  try_with k ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | GetSize -> Option.some @@ fun (k : (a, _) continuation) ->
            continue k size
          | _ -> None
    }

let bind_var k =
  let size = Effect.perform GetSize in
  run ~size:(size + 1) @@ fun () -> k size

let level_to_index lvl =
  let size = Effect.perform GetSize in
  size - lvl - 1
