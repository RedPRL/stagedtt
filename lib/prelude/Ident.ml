type t =
  | User of Yuujinchou.Trie.path
  | Anon

let pp fmt =
  function
  | User path ->
    let pp_sep fmt () = Format.pp_print_string fmt "::" in
    Format.pp_print_list ~pp_sep Format.pp_print_string fmt path
  | Anon -> Format.pp_print_string fmt "_"

let user str =
  User [str]

let user_string path =
  String.concat "::" path
