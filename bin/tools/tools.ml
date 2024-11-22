open! Import

module Exec = struct
  let doc = "Command group for running wrapped tools."
  let info = Cmd.info ~doc "exec"
  let group = Cmd.group info [ Ocamlformat.Exec.command; Ocamllsp.Exec.command ]
end

module Install = struct
  let doc = "Command group for installing wrapped tools."
  let info = Cmd.info ~doc "install"
  let group = Cmd.group info [ Ocamlformat.Install.command; Ocamllsp.Install.command ]
end

let doc = "Command group for wrapped tools."
let info = Cmd.info ~doc "tools"
let group = Cmd.group info [ Exec.group; Install.group ]
