open Frontend
open Cmdliner

type load_options =
  { input : [`Stdin | `File of string]
  }

let main {input} =
  Execute.load input

let load_cmd =
  let doc = "Load a stagedtt vernacular file." in
  let load_info = Cmd.info "load" ~doc in
  let parse_input_args = Term.(app @@ const @@ fun str -> { input = if str = "-" then `Stdin else `File str }) in
  let input_doc = "The file to typecheck. When $(docv) is -, read standard input." in
  let input_arg = Arg.(parse_input_args & value & pos ~rev:true 0 string "-" & info [] ~doc:input_doc ~docv:"FILE") in
  Cmd.v load_info @@ Term.(const main $ input_arg)

let stagedtt_info =
  let doc = "A Staged type theory" in
  let err_exit = Cmd.Exit.info ~doc:"on ill-formed types or terms." 1 in
  (* [TODO: Reed M, 02/05/2022] We should grab the version from a git hash... *)
  Cmd.info "stagedtt" ~version:[%git_hash] ~doc
    ~exits:(err_exit :: Cmd.Exit.defaults)

let () =
  exit (Cmd.eval ~catch:true ~err:Format.std_formatter @@ Cmd.group stagedtt_info [load_cmd])
