module CS = Elaborator.Syntax

type command =
  | Declare of { ident : string; tp : CS.t option; tm : CS.t }
  | Fail of { ident : string; tp : CS.t option; tm : CS.t }
  | Normalize of { tm : CS.t }
  | Stage of { tm : CS.t }
  | Print of string
  | Quit
