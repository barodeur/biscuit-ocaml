open Cmdliner

let cmd = Cmd.group (Cmd.info "biscuit") [ Validate.cmd ]
let () = exit (Cmd.eval cmd)
