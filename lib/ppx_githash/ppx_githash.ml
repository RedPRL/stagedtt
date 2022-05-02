open Ppxlib

let name = "git_hash"

let expand ~loc ~path:_ =
  let in_channel = Unix.open_process_args_in "git" [| "git"; "describe"; "--tags"; "--always"; "--long"; "--dirty" |] in
  let tag = input_line in_channel in
  close_in in_channel;
  Ast_builder.Default.estring tag ~loc

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]
