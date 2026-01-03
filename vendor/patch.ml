open Import
open Common

let write_patch_file ~patch_file content =
  Console.print_status "Writing patch to %s..." (Path.to_string patch_file) ~f:(fun () ->
    Path.mkdir_p (Path.parent_exn patch_file);
    Io.write_file patch_file content;
    Console.printf_always "Wrote patch %s" (Path.to_string patch_file))
;;

let generate_patch_for_package ~dry_run package_name package_specs =
  Console.print_status "Generating patch for %s" package_name ~f:(fun () ->
    with_temp_dir
    @@ fun temp_dir ->
    let temp_package_dir = Path.L.relative temp_dir [ "vendor_copy"; package_name ] in
    Path.mkdir_p temp_package_dir;
    let* () =
      Fetch.apply_copy_rules_for_package
        ~repo_dir:temp_dir
        ~target_dir:temp_package_dir
        package_name
        package_specs
    in
    let* () =
      Fiber.parallel_iter package_specs ~f:(fun { Packages.preserve_rules; _ } ->
        Fiber.parallel_iter preserve_rules ~f:(fun path ->
          let vendor_file =
            Path.L.relative (Lazy.force vendor_dir) [ package_name; path ]
          in
          let+ () = Fiber.return () in
          if Path.exists vendor_file
          then Io.copy_file ~src:vendor_file ~dst:(Path.relative temp_package_dir path) ()
          else
            User_error.raise
              [ Pp.textf
                  "File %S does not exist and cannot be preserved."
                  (Path.to_string vendor_file)
              ]))
    in
    let vendor_package_path = Path.relative (Lazy.force vendor_dir) package_name in
    let+ diff_output =
      let+ raw_diff_output =
        Git.diff_no_index ~path1:temp_package_dir ~path2:vendor_package_path
      in
      (* Fix diff paths by replacing absolute path prefixes with clean vendor paths *)
      raw_diff_output
      (* Replace diff paths: a/temp_path -> a/vendor_path and b/vendor_path stays *)
      |> Re.replace_string
           (Re.compile (Re.str ("a" ^ Path.to_string temp_package_dir)))
           ~by:("a/vendor/" ^ package_name)
      |> Re.replace_string
           (Re.compile (Re.str ("b" ^ Path.to_string vendor_package_path)))
           ~by:("b/vendor/" ^ package_name)
      (* Git will generate index lines which we don't need. We make sure to avoid
       [split_lines] here as it malforms the patch file. *)
      |> String.split_on_char ~sep:'\n'
      |> List.filter ~f:(fun line -> not (String.starts_with line ~prefix:"index "))
      |> String.concat ~sep:"\n"
    in
    if String.trim diff_output = ""
    then Console.printf "No differences found - current state matches fetched state."
    else (
      let patch_file =
        Path.L.relative (Lazy.force vendor_dir) [ "patches"; package_name ^ ".patch" ]
      in
      if Path.exists patch_file
      then
        if String.equal (Io.read_file patch_file) diff_output
        then Console.printf "Patch for %s is up to date" package_name
        else if dry_run
        then
          User_error.raise
            [ Pp.textf
                "Generated patch differs from existing patch file %s."
                (Path.to_string patch_file)
            ]
            ~hints:[ Pp.textf "Run the `patch` command to update the patch file" ]
        else write_patch_file ~patch_file diff_output
      else if dry_run
      then Console.printf "No patch file for %s (this is fine)" package_name
      else write_patch_file ~patch_file diff_output))
;;

let patch_command =
  let open Cmdliner in
  Cmd.v (Cmd.info "patch" ~doc:"Generate patches for packages")
  @@
  let+ options = package_term in
  cmd_foreach_pkg "Generating patch for package:" generate_patch_for_package options
;;

let lint_command =
  let open Cmdliner in
  Cmd.v (Cmd.info "lint" ~doc:"Check patch integrity")
  @@ let+ options = package_term in
     cmd_foreach_pkg
       "Checking patch for package:"
       generate_patch_for_package
       { options with dry_run = true }
;;
