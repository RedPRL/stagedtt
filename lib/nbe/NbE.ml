open Eff

let eval ~stage ~env tm =
  Staging.run ~stage @@ fun () ->
  Eval.eval ~env tm
let eval_tp ~stage ~env tp =
  Staging.run ~stage @@ fun () ->
  Eval.eval_tp ~env tp

let do_el ~stage tm =
  Staging.run ~stage @@ fun () ->
  Eval.do_el tm

let quote ~size tm =
  Quoting.run ~size @@ fun () ->
  Quote.quote tm
let quote_tp ~size tp =
  Quoting.run ~size @@ fun () ->
  Quote.quote_tp tp

let equate = Conversion.equate
let equate_tp = Conversion.equate_tp

exception NotConvertible = Conversion.NotConvertible

let graft_value = Eval.graft_value
let graft_tp = Eval.graft_tp

let inst_tm_clo ~stage clo v =
  Staging.run ~stage @@ fun () ->
  Eval.inst_tm_clo clo v

let inst_tp_clo ~stage clo v =
  Staging.run ~stage @@ fun () ->
  Eval.inst_tp_clo clo v

