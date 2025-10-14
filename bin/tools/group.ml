open! Import

module Exec = struct
  let doc = "Command group for running wrapped tools."
  let info = Cmd.info ~doc "exec"

  let group =
    Cmd.group
      info
      (List.map
         [ Ocamlformat; Ocamllsp; Ocamlearlybird; Odig; Opam_publish; Dune_release ]
         ~f:Tools_common.exec_command)
  ;;
end

module Install = struct
  let doc = "Command group for installing wrapped tools."
  let info = Cmd.info ~doc "install"

  let group =
    Cmd.group info (List.map Dune_pkg.Dev_tool.all ~f:Tools_common.install_command)
  ;;
end

module Which = struct
  let doc = "Command group for printing the path to wrapped tools."
  let info = Cmd.info ~doc "which"

  let group =
    Cmd.group info (List.map Dune_pkg.Dev_tool.all ~f:Tools_common.which_command)
  ;;
end

let doc = "Command group for wrapped tools."
let info = Cmd.info ~doc "tools"

let group =
  Cmd.group info [ Exec.group; Install.group; Which.group; Tools_common.env_command ]
;;
