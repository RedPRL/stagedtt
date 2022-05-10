type path = Yuujinchou.Trie.path

type t =
  | User of path
  | Anon

let pp_path fmt path =
  let pp_sep fmt () = Format.pp_print_string fmt "::" in
  Format.pp_print_list ~pp_sep Format.pp_print_string fmt path

let pp fmt =
  function
  | User path ->
    pp_path fmt path
  | Anon -> Format.pp_print_string fmt "_"

let user str =
  User [str]

let user_string path =
  String.concat "::" path

let equal ident0 ident1 =
  match (ident0, ident1) with
  | Anon, Anon -> true
  | User path0, User path1 -> List.equal String.equal path0 path1
  | _, _ -> false

let hash ident =
  match ident with
  | User path -> String.hash (user_string path)
  | Anon -> 0
