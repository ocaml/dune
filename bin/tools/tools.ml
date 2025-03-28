open! Import

module Exec = struct
  let doc = "Command group for running wrapped tools."
  let info = Cmd.info ~doc "exec"
  let group = Cmd.group info [ Ocamlformat.Exec.command; Ocamllsp.Exec.command ]
end

module Which = struct
  let doc = "Command group for printing the path to wrapped tools."
  let info = Cmd.info ~doc "which"
  let group = Cmd.group info [ Ocamlformat.Which.command; Ocamllsp.Which.command ]
end

let doc = "Command group for wrapped tools."
let info = Cmd.info ~doc "tools"
let group = Cmd.group info [ Exec.group; Which.group ]
