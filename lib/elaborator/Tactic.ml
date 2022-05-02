open Core

module S = Syntax
module D = Domain

module Tp : sig
  type tac

  val rule : ?name:string -> (unit -> S.tp) -> tac
  val run : tac -> S.tp
end =
struct
  type tac =
    | Tp of string * (unit -> S.tp)

  let rule ?(name = "") k =
    Tp (name, k)

  let run =
    function
    | Tp (_, k) -> k ()
end

module rec Chk : sig
  type tac

  val rule : ?name:string -> (D.tp -> S.t) -> tac
  val run : tac -> D.tp -> S.t
  val syn : Syn.tac -> tac
end =
struct
  type tac =
    | Chk of string * (D.tp -> S.t)

  let run =
    function
    | Chk (_, tac) -> tac

  let rule ?(name = "") tac = Chk(name, tac)

  let syn tac =
    rule @@ fun tp0 ->
    let (tm, tp1) = Syn.run tac in
    Conversion.equate_tp ~size:0 tp0 tp1;
    tm
end

and Syn : sig
  type tac

  val rule : ?name:string -> (unit -> S.t * D.tp) -> tac
  val run : tac -> S.t * D.tp
  val ann : Chk.tac -> D.tp -> tac
end =
struct
  type tac =
    | Syn of string * (unit -> S.t * D.tp)

  let run =
    function
    | Syn (_, k) -> k ()

  let rule ?(name = "") k =
    Syn (name, k)

  let ann tac tp =
    rule @@ fun () ->
    Chk.run tac tp, tp
end
