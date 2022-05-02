open Command

type status =
  | Continue
  | Quit

let not_implemented string =
  let message = Format.asprintf "%s is not implemented yet!" string in
  let diag = Diagnostic.warning ~code:"W0001" message in
  Diagnostic.pp Format.err_formatter diag;
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
  match Loader.load input with
  | Ok cmds -> exec cmds
  | Error diagnostic -> Diagnostic.pp Format.err_formatter diagnostic
