open Data

include Domain

let local lvl =
  Neu { hd = Local lvl; spine = [] }

let global nm v =
  Neu { hd = Global (nm, v); spine = [] }
