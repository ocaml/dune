open! Import
module Package_universe = Dune_pkg.Package_universe
module Lock_dir = Dune_pkg.Lock_dir
module Opam_repo = Dune_pkg.Opam_repo
module Package_version = Dune_pkg.Package_version
module Opam_solver = Dune_pkg.Opam_solver

let info =
  let doc = "Validate that a lockdir contains a solution for local packages" in
  let man = [ `S "DESCRIPTION"; `P doc ] in
  Cmd.info "validate-lockdir" ~doc ~man
;;

(* CR-someday alizter: The logic here is a little more complicated than it needs
   to be and can be simplified. *)

let enumerate_lock_dirs_by_path ~lock_dirs () =
  let open Fiber.O in
  let+ lock_dir_paths =
    Memo.run (Workspace.workspace ())
    >>| Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs
  in
  List.filter_map lock_dir_paths ~f:(fun lock_dir_path ->
    if Path.exists (Path.source lock_dir_path)
    then (
      try Some (Ok (lock_dir_path, Lock_dir.read_disk lock_dir_path)) with
      | User_error.E e -> Some (Error (lock_dir_path, `Parse_error e)))
    else None)
;;

let validate_lock_dirs ~lock_dirs () =
  let open Fiber.O in
  let+ lock_dirs_by_path = enumerate_lock_dirs_by_path ~lock_dirs ()
  and+ local_packages = Pkg_common.find_local_packages
  and+ workspace = Memo.run (Workspace.workspace ()) in
  if List.is_empty lock_dirs_by_path
  then Console.print [ Pp.text "No lockdirs to validate." ]
  else (
    match
      List.filter_map lock_dirs_by_path ~f:(function
        | Error e -> Some e
        | Ok (path, lock_dir) ->
          let default_local_package_version =
            Workspace.default_local_package_version_of_lock_dir_path workspace path
          in
          (match
             Package_universe.create
               ~default_local_package_version
               local_packages
               lock_dir
           with
           | Ok _ -> None
           | Error e -> Some (path, `Lock_dir_out_of_sync e)))
    with
    | [] -> ()
    | errors_by_path ->
      List.iter errors_by_path ~f:(fun (path, error) ->
        match error with
        | `Parse_error error ->
          User_message.prerr
            (User_message.make
               [ Pp.textf
                   "Failed to parse lockdir %s:"
                   (Path.Source.to_string_maybe_quoted path)
               ; User_message.pp error
               ])
        | `Lock_dir_out_of_sync error ->
          User_message.prerr
            (User_message.make
               [ Pp.textf
                   "Lockdir %s does not contain a solution for local packages:"
                   (Path.Source.to_string path)
               ]);
          User_message.prerr error);
      User_error.raise
        [ Pp.text "Some lockdirs do not contain solutions for local packages:"
        ; Pp.enumerate errors_by_path ~f:(fun (path, _) ->
            Pp.text (Path.Source.to_string path))
        ])
;;

let term =
  let+ builder = Common.Builder.term
  and+ lock_dirs = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config @@ validate_lock_dirs ~lock_dirs
;;

let command = Cmd.v info term
