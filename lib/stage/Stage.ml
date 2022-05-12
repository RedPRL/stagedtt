open Prelude
open Core

let stage ~tm_stage tm = Quote.quote_inner ~size:0 @@ Eval.eval_inner ~tm_stage tm

let eval_inner = Eval.eval_inner
let eval_outer = Eval.eval_outer

let quote_inner = Quote.quote_inner
