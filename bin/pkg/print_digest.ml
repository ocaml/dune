open Import

let term =
  let+ builder = Common.Builder.term
  and+ package_name =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:(Some "The name of the package") ~docv:"PACKAGE")
  and+ context_name =
    Common.context_arg ~doc:(Some "Context used to determine lockdir")
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  let package_name =
    match Package_name.of_string_opt package_name with
    | Some package_name -> package_name
    | None -> User_error.raise [ Pp.textf "Invalid package name: %S" package_name ]
  in
  Scheduler_setup.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    let+ pkg_digest_opt =
      build_exn (fun () ->
        let open Memo.O in
        let* lock_dir_active = Dune_rules.Lock_dir.lock_dir_active context_name in
        if lock_dir_active
        then
          Dune_rules.Pkg_rules.pkg_digest_of_project_dependency context_name package_name
        else
          User_error.raise
            [ Pp.textf
                "Lock directory is not active for context %S."
                (Context_name.to_string context_name)
            ])
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
