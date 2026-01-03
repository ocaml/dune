open Import
open Common

let apply_patches_for_package package_name =
  let patch_file =
    Path.L.relative (Lazy.force vendor_dir) [ "patches"; package_name ^ ".patch" ]
  in
  if Path.exists patch_file
  then
    Console.print_status "Applying patch for %s" package_name ~f:(fun () ->
      let+ () =
        Dune_patch.For_tests.exec
          !Dune_engine.Clflags.display
          ~patch:patch_file
          ~dir:Path.root
          ~stderr:(if !Console.verbose then Process.Io.stderr else Process.Io.null Out)
      in
      Console.printf "Patch applied successfully")
  else
    let+ () = Fiber.return () in
    Console.printf "No patch file found for %s" package_name
;;

let apply_copy_rules_for_package ~repo_dir ~target_dir package_name package_specs =
  Fiber.parallel_iter
    package_specs
    ~f:(fun { Packages.source = Git { url; revision; build_cmd }; copy_rules; _ } ->
      let* fetched_dir =
        Path.relative repo_dir (Filename.basename url)
        |> fetch_git ~url ~revision ~build_cmd
      in
      Fiber.parallel_iter copy_rules ~f:(fun copy_rule ->
        Packages.Copy_rule.map
          copy_rule
          ~f_src:(Path.relative fetched_dir)
          ~f_dst:(Path.relative target_dir)
        |> execute_copy_rule ~name:package_name))
;;

let update_package ~dry_run:_ package_name package_specs =
  Console.print_status "Fetching %s" package_name ~f:(fun () ->
    with_temp_dir
    @@ fun temp_dir ->
    let vendor_package_dir = Path.relative (Lazy.force vendor_dir) package_name in
    Path.rm_rf vendor_package_dir;
    Path.mkdir_p vendor_package_dir;
    let* () =
      apply_copy_rules_for_package
        ~repo_dir:temp_dir
        ~target_dir:vendor_package_dir
        package_name
        package_specs
    in
    let* () =
      Fiber.parallel_iter package_specs ~f:(fun { Packages.preserve_rules; _ } ->
        Fiber.parallel_iter preserve_rules ~f:(fun path ->
          Git.checkout_file ~file_path:(Path.relative vendor_package_dir path)))
    in
    let+ () = apply_patches_for_package package_name in
    Console.printf_always "Updated %s" package_name)
;;

let fetch_command =
  let open Cmdliner in
  Cmd.v (Cmd.info "fetch" ~doc:"Fetch packages")
  @@
  let+ options = package_term in
  cmd_foreach_pkg ~supports_dry_run:false "Fetching package:" update_package options
;;
