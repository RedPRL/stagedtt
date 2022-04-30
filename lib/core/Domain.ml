open Data

include Domain

let local lvl =
  Neu { hd = Local lvl; spine = [] }

let global nm v =
  Neu { hd = Global (nm, v); spine = [] }


let push_frm (neu : neu) (frm : frm) ~(unfold : t -> t) : neu =
  match neu.hd with
  | Global (nm, v) ->
    { hd = Global (nm, Lazy.map unfold v); spine = frm :: neu.spine }
  | _ ->
    { hd = neu.hd; spine = frm :: neu.spine }
