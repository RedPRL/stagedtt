let stage ~stage ~tm_stage tm = 
  Quote.quote_inner @@ Eval.eval_inner ~stage ~tm_stage tm
