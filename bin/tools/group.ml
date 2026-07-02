open Import

let subcommand ~prefix ~doc f =
  let info = Cmd.info ~doc prefix in
  Cmd.group info (List.map Dune_pkg.Dev_tool.all ~f)
;;

let exec =
  subcommand
    ~prefix:"exec"
    ~doc:"Command group for running wrapped tools."
    Tools_common.exec_command
;;

let install =
  subcommand
    ~prefix:"install"
    ~doc:"Command group for installing wrapped tools."
    Tools_common.install_command
;;

let which =
  subcommand
    ~prefix:"which"
    ~doc:"Command group for printing the path to wrapped tools."
    Tools_common.which_command
;;

let doc = "Command group for wrapped tools."
let info = Cmd.info ~doc "tools"
let group = Cmd.group info [ exec; install; which; Tools_common.env_command ]
