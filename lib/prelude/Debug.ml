let [@inline] dump_callstack ?(depth = 100) msg  =
  Format.eprintf "[DEBUG] %s@." msg;
  let callstack = Printexc.get_callstack depth in
  Printexc.print_raw_backtrace Out_channel.stderr callstack

let debug_enabled = ref false

let debug_mode is_debug =
  debug_enabled := is_debug

let is_debug_mode () =
  !debug_enabled

let debug_formatter =
  let out buf pos len =
    if !debug_enabled then
      Stdlib.output_substring Stdlib.stderr buf pos len
    else
      ()
  in
  let flush () = Stdlib.flush Stdlib.stderr in
  Format.make_formatter out flush

let print ~file (fmt : ('a, Format.formatter, unit) format) =
  Format.fprintf debug_formatter "DEBUG[%s] " file;
  Format.fprintf debug_formatter fmt
