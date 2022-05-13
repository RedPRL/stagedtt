open Lexing

type t =
  { start : position;
    stop : position }

let filename span =
  span.start.pos_fname

let start_row span =
  span.start.pos_lnum

let start_column span =
  span.start.pos_cnum - span.start.pos_bol

let stop_column span =
  span.stop.pos_cnum - span.stop.pos_bol

let span_size span =
  span.stop.pos_cnum - span.start.pos_cnum
