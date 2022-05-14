open Prelude
module CS = Elaborator.Syntax

type command =
  | Def of { ident : Ident.t; tp : CS.t option; tm : CS.t }
  | DefStaged of { ident : Ident.t; tp : CS.t option; tm : CS.t }
  | Fail of { message : string; tp : CS.t option; tm : CS.t }
  | Normalize of { tm : CS.t }
  | Stage of Ident.path
  | Print of Ident.path
  | Debug of bool
  | Quit
