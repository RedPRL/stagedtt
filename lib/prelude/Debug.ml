let [@inline] dump_callstack ?(depth = 100) msg  =
  Format.eprintf "[DEBUG] %s@." msg;
  let callstack = Printexc.get_callstack depth in
  Printexc.print_raw_backtrace Out_channel.stderr callstack
