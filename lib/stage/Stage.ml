open Eff

let eval_inner ~stage tm =
  Staging.run ~stage @@ fun () ->
  Eval.eval_inner tm

let eval_outer ~stage tm =
  Staging.run ~stage @@ fun () ->
  Eval.eval_outer tm

let quote_inner ~size tm =
  Quoting.run ~size @@ fun () ->
  Quote.quote_inner tm

let stage ~stage tm =
  quote_inner ~size:0 @@ eval_inner ~stage tm

module Effectful =
struct
  let eval_inner = Eval.eval_inner
  let eval_outer = Eval.eval_outer
end
