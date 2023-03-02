open Core

let channel_of_path = function
  | "-" -> In_channel.stdin
  | _ as path -> In_channel.create path

let validate path =
  let channel = channel_of_path path in
  let result = Biscuit_parser.parse_channel channel in
  let _ =
    result
    |> Result.map_error ~f:(function Biscuit_parser.ParserError msg -> msg)
    |> Result.ok_or_failwith
  in
  print_endline "Token is valid!";
  ()

module Cmd = struct
  open Cmdliner

  let path_arg = Arg.(value & pos 0 string "-" & info [])
  let term = Term.(const validate $ path_arg)
  let t = Cmd.v (Cmd.info "validate") term
end

let cmd = Cmd.t
