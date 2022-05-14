open Core
open Eff

module S = Syntax
module I = Inner
module O = Outer

let rec quote_inner tm =
  match tm with
  | I.Local lvl ->
    S.Local (Quoting.level_to_index lvl)
  | I.Global gbl ->
    S.Global gbl
  | I.Hole nm ->
    S.Hole nm
  | I.Lam (x, body) ->
    S.Lam (x, Quoting.bind_var @@ fun _ -> quote_inner body)
  | I.Ap (fn, arg) ->
    S.Ap (quote_inner fn, quote_inner arg)
  | I.Quote (tm) ->
    S.Quote (quote_inner tm)
  | I.Splice tm ->
    S.Splice (quote_inner tm)
  | I.CodePi (base, fam) ->
    S.CodePi (quote_inner base, quote_inner fam)
  | I.CodeExpr tm ->
    S.CodeExpr (quote_inner tm)
  | I.CodeUniv stage ->
    S.CodeUniv stage
