open Command

type status =
  | Continue
  | Quit

let not_implemented string =
  Format.eprintf "Warning: %s is not implemented yet!" string;
  Continue

let exec_command : command -> status =
  function
  | Declare _ ->
    not_implemented "def"
  | Fail _ ->
    not_implemented "#fail"
  | Normalize _ ->
    not_implemented "#normalize"
  | Stage _ ->
    not_implemented "#stage"
  | Print _ ->
    not_implemented "#print"
  | Quit -> Quit

let rec exec : command list -> unit =
  function
  | [] -> ()
  | cmd :: cmds ->
    begin
      match exec_command cmd with
      | Continue -> exec cmds
      | Quit -> ()
    end

let load input =
  let cmds = Loader.load input in
  exec cmds
