open Import
module Lock_dir = Dune_pkg.Lock_dir
module Local_package = Dune_pkg.Local_package

module Lock = struct
  let term =
    let+ lock_dir_paths =
      Arg.(
        value
        & pos_all string []
        & info
            ~doc:
              "The paths of the the lock directories to be inspected. Defaults to the \
               lock directory specified in the default context."
            ~docv:"LOCKDIRS"
            [])
    in
    let lock_dir_paths =
      match List.map lock_dir_paths ~f:Path.Source.of_string with
      | [] -> [ Lock_dir.default_path ]
      | lock_dir_paths -> lock_dir_paths
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

let command =
  let doc = "Subcommands related to package management" in
  let info = Cmd.info ~doc "pkg" in
  Cmd.group info [ Lock.command; Dependency_hash.command ]
;;
