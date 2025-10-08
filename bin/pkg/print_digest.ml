open Import

let term =
  let+ builder = Common.Builder.term
  and+ package_name =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"The name of the package" ~docv:"PACKAGE")
  and+ context_name = Common.context_arg ~doc:"Context used to determine lockdir" in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  let package_name = Package_name.of_string package_name in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    let+ pkg_digest_opt =
      build_exn (fun () ->
        Dune_rules.Pkg_rules.pkg_digest_of_project_dependency context_name package_name)
    in
    match pkg_digest_opt with
    | Some pkg_digest ->
      print_endline (Dune_rules.Pkg_rules.Pkg_digest.to_string pkg_digest)
    | None ->
      User_error.raise
        [ Pp.textf
            "The project does not depend on the package %S."
            (Package_name.to_string package_name)
        ])
;;

let info =
  let doc = "Print the digest of a package in the project's lockdir." in
  Cmd.info "print-digest" ~doc
;;

let command = Cmd.v info term
