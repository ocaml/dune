open Import
module Toolchain = Dune_pkg.Toolchain

module Version = struct
  include Toolchain.Version

  let conv = Arg.enum (List.map all ~f:(fun version -> to_string version, version))
end

module Get = struct
  let run version = Toolchain.get ~log:`Always version

  let term =
    let+ builder = Common.Builder.term
    and+ version =
      Arg.(required & pos 0 (some Version.conv) None & info [] ~docv:"VERSION")
    in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config (fun () -> run version)
  ;;

  let info = Cmd.info "get" ~doc:"Install a given toolchain version"
  let cmd = Cmd.v info term
end

let info =
  let doc = "Manage OCaml compiler toolchains" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Commands for managing compiler toolchains|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.info "x-experimental-toolchain" ~doc ~man
;;

let group = Cmd.group info [ Get.cmd ]
