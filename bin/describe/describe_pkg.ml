open Import
module Lock_dir = Dune_pkg.Lock_dir
module Local_package = Dune_pkg.Local_package

module Show_lock = struct
  let print_lock lock_dir_arg () =
    let open Fiber.O in
    let+ lock_dir_paths =
      Memo.run (Workspace.workspace ())
      >>| Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dir_arg
    in
    Console.print
    @@ List.map lock_dir_paths ~f:(fun lock_dir_path ->
      let lock_dir = Lock_dir.read_disk lock_dir_path in
      Pp.concat
        ~sep:Pp.space
        [ Pp.hovbox
          @@ Pp.textf "Contents of %s:" (Path.Source.to_string_maybe_quoted lock_dir_path)
        ; Pkg_common.pp_packages
            (Package_name.Map.to_list_map ~f:(fun _ pkg -> pkg) lock_dir.packages)
        ]
      |> Pp.vbox)
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ lock_dir_arg = Pkg_common.Lock_dirs_arg.term in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config @@ print_lock lock_dir_arg
  ;;

  let command =
    let doc = "Display packages in a lock file" in
    let info = Cmd.info ~doc "lock" in
    Cmd.v info term
  ;;
end

module Dependency_hash = struct
  let print_local_packages_hash () =
    let open Fiber.O in
    let+ local_packages =
      Pkg_common.find_local_packages
      |> Memo.run
      >>| Package_name.Map.values
      >>| List.map ~f:Local_package.for_solver
    in
    match
      Local_package.(
        For_solver.list_non_local_dependency_set local_packages |> Dependency_set.hash)
    with
    | None -> User_error.raise [ Pp.text "No non-local dependencies" ]
    | Some dependency_hash ->
      print_endline (Local_package.Dependency_hash.to_string dependency_hash)
  ;;

  let term =
    let+ builder = Common.Builder.term in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config print_local_packages_hash
  ;;

  let info =
    let doc =
      "Print the hash of the project's non-local dependencies such as what would appear \
       in the \"dependency_hash\" field of a a lock.dune file."
    in
    Cmd.info "dependency-hash" ~doc
  ;;

  let command = Cmd.v info term
end

module List_locked_dependencies = struct
  module Package_universe = Dune_pkg.Package_universe
  module Lock_dir = Dune_pkg.Lock_dir
  module Opam_repo = Dune_pkg.Opam_repo
  module Package_version = Dune_pkg.Package_version
  module Opam_solver = Dune_pkg.Opam_solver

  let info =
    let doc = "List the dependencies locked by a lockdir" in
    let man = [ `S "DESCRIPTION"; `P "List the dependencies locked by a lockdir" ] in
    Cmd.info "list-locked-dependencies" ~doc ~man
  ;;

  let package_deps_in_lock_dir_pp package_universe package_name ~transitive =
    let traverse, traverse_word =
      if transitive then `Transitive, "Transitive" else `Immediate, "Immediate"
    in
    let opam_package =
      Package_universe.opam_package_of_package package_universe package_name
    in
    let list_dependencies which =
      Package_universe.opam_package_dependencies_of_package
        package_universe
        package_name
        ~which
        ~traverse
    in
    Pp.concat
      ~sep:Pp.cut
      [ Pp.hbox
          (Pp.textf
             "%s dependencies of local package %s"
             traverse_word
             (OpamPackage.to_string opam_package))
      ; Pp.enumerate (list_dependencies `Non_test) ~f:(fun opam_package ->
          Pp.text (OpamPackage.to_string opam_package))
      ; Pp.enumerate (list_dependencies `Test_only) ~f:(fun opam_package ->
          Pp.textf "%s (test only)" (OpamPackage.to_string opam_package))
      ]
    |> Pp.vbox
  ;;

  let enumerate_lock_dirs_by_path workspace ~lock_dirs =
    let lock_dirs = Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs workspace in
    List.filter_map lock_dirs ~f:(fun lock_dir_path ->
      if Path.exists (Path.source lock_dir_path)
      then (
        try Some (lock_dir_path, Lock_dir.read_disk lock_dir_path) with
        | User_error.E e ->
          User_warning.emit
            [ Pp.textf
                "Failed to parse lockdir %s:"
                (Path.Source.to_string_maybe_quoted lock_dir_path)
            ; User_message.pp e
            ];
          None)
      else None)
  ;;

  let list_locked_dependencies ~transitive ~lock_dirs () =
    let open Fiber.O in
    let+ lock_dirs_by_path, local_packages =
      let open Memo.O in
      Memo.both
        (Workspace.workspace () >>| enumerate_lock_dirs_by_path ~lock_dirs)
        Pkg_common.find_local_packages
      |> Memo.run
    in
    let pp =
      Pp.concat
        ~sep:Pp.cut
        (List.map lock_dirs_by_path ~f:(fun (lock_dir_path, lock_dir) ->
           let package_universe =
             Package_universe.create local_packages lock_dir |> User_error.ok_exn
           in
           Pp.vbox
             (Pp.concat
                ~sep:Pp.cut
                [ Pp.hbox
                    (Pp.textf
                       "Dependencies of local packages locked in %s"
                       (Path.Source.to_string_maybe_quoted lock_dir_path))
                ; Pp.enumerate
                    (Package_name.Map.keys local_packages)
                    ~f:(package_deps_in_lock_dir_pp package_universe ~transitive)
                  |> Pp.box
                ])))
      |> Pp.vbox
    in
    Console.print [ pp ]
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ transitive =
      Arg.(
        value
        & flag
        & info
            [ "transitive" ]
            ~doc:
              "Display transitive dependencies (by default only immediate dependencies \
               are displayed)")
    and+ lock_dirs = Pkg_common.Lock_dirs_arg.term in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config @@ list_locked_dependencies ~transitive ~lock_dirs
  ;;

  let command = Cmd.v info term
end

let command =
  let doc = "Subcommands related to package management" in
  let info = Cmd.info ~doc "pkg" in
  Cmd.group
    info
    [ Show_lock.command; List_locked_dependencies.command; Dependency_hash.command ]
;;
