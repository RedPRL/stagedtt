open Core

module S = Syntax
module I = Inner
module O = Outer

open struct
  module Eff = Algaeff.Reader.Make (struct type env = int end)

  let bind_var (f : I.t -> 'a) : 'a =
    Eff.scope (fun size -> size + 1) @@ fun () ->
    let size = Eff.read () in 
    f (I.Local (size - 1))

  let quote_var lvl =
    let n = Eff.read () in
    n - (lvl + 1)

  let rec quote_inner tm =
    match tm with
    | I.Local lvl ->
      S.Local (quote_var lvl)
    | I.Global (path, unf) ->
      S.Global (path, unf)
    | I.Staged (path, sunf, unf) ->
      S.Staged (path, sunf, unf)
    | I.Lam (x, body) ->
      S.Lam (x, bind_var @@ fun _ -> quote_inner body)
    | I.Ap (fn, arg) ->
      S.Ap (quote_inner fn, quote_inner arg)
    | I.Quote (tm) ->
      S.Quote (quote_inner tm)
    | I.Splice tm ->
      S.Splice (quote_inner tm)
    | I.CodePi (base, fam) ->
      S.CodePi (quote_inner base, quote_inner fam)
    | I.CodeUniv stage ->
      S.CodeUniv stage
end

let quote_inner tm =
  Eff.run ~env:0 @@ fun () -> quote_inner tm
