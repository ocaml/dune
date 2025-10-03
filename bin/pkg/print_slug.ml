open Import

let term =
  let+ builder = Common.Builder.term
  and+ package_name =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~doc:"The name of the package" ~docv:"PACKAGE")
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  let package_name = Package_name.of_string package_name in
  let context_name = Context_name.default in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    let* lock_dir_path_opt =
      Memo.run (Dune_rules.Lock_dir.get_path_source context_name)
    in
    match lock_dir_path_opt with
    | None -> User_error.raise [ Pp.textf "No lockdir found" ]
    | Some lock_dir_path ->
      let lock_dir = Dune_pkg.Lock_dir.read_disk_exn (Path.source lock_dir_path) in
      let+ platform = Pkg_common.poll_solver_env_from_current_system () in
      let packages_by_name =
        Dune_pkg.Lock_dir.Packages.pkgs_on_platform_by_name lock_dir.packages ~platform
      in
      (match Package_name.Map.find packages_by_name package_name with
       | None ->
         User_error.raise
           [ Pp.textf "No such package: %s" (Package_name.to_string package_name) ]
       | Some package ->
         let slug = Dune_pkg.Lock_dir.Pkg.slug package in
         print_endline (Dune_pkg.Lock_dir.Pkg_slug.to_string slug)))
;;

let info =
  let doc = "Print the slug of a package in the project's lockdir." in
  Cmd.info "print-slug" ~doc
;;

let command = Cmd.v info term
