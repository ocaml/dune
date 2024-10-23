open! Import

module Exec = struct
  let doc = "Command group for running wrapped tools."
  let info = Cmd.info ~doc "exec"
  let group = Cmd.group info [ Ocamlformat.command; Ocamllsp.command ]
end

let doc = "Command group for wrapped tools."
let info = Cmd.info ~doc "tools"
let group = Cmd.group info [ Exec.group ]
