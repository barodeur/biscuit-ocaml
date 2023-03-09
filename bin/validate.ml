open Core
open Core.Result.Let_syntax

let channel_of_path = function
  | "-" -> In_channel.stdin
  | _ as path -> In_channel.create path

let validate path =
  let channel = channel_of_path path in
  let result = Biscuit_parser.parse_channel channel in
  let%bind _ = result |> Result.map_error ~f:Biscuit_parser.Error.to_string in
  print_endline "Token is valid!";
  Ok ()

module Cmd = struct
  open Cmdliner

  let path_arg = Arg.(value & pos 0 string "-" & info [])
  let term = Term.(const validate $ path_arg)
  let t = Cmd.v (Cmd.info "validate") term
end

let cmd = Cmd.t
