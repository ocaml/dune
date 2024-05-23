open Import
module Toolchain = Dune_pkg.Toolchain

module Get = struct
  let run package =
    let open Fiber.O in
    let* available_compiler_packages =
      Toolchain.Available_compilers.load_upstream_opam_repo ()
    in
    let package_name =
      Dune_pkg.Package_name.of_opam_package_name (OpamPackage.name package)
    in
    let package_version =
      Dune_pkg.Package_version.of_opam_package_version (OpamPackage.version package)
    in
    match
      Toolchain.Available_compilers.find_package
        available_compiler_packages
        package_name
        package_version
    with
    | Some compiler_package -> Toolchain.Compiler.get ~log_when:`Always compiler_package
    | None ->
      User_error.raise
        [ Pp.textf
            "Unknown compiler package %s.%s"
            (Package_name.to_string package_name)
            (Package_version.to_string package_version)
        ]
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ package = Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE") in
    let common, config = Common.init builder in
    let package = OpamPackage.of_string package in
    Scheduler.go ~common ~config (fun () -> run package)
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
