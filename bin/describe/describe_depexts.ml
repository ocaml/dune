open Import
open Pkg_common
module Lock_dir = Dune_pkg.Lock_dir

let enumerate_lock_dirs_by_path workspace ~lock_dirs =
  let lock_dirs = Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs workspace in
  List.filter_map lock_dirs ~f:(fun lock_dir_path ->
    if Path.exists (Path.source lock_dir_path)
    then (
      try Some (Lock_dir.read_disk_exn lock_dir_path) with
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

let print_depexts ~lock_dirs_arg =
  let open Fiber.O in
  let open Lock_dir in
  let+ workspace = Memo.run (Workspace.workspace ()) in
  let depexts =
    enumerate_lock_dirs_by_path workspace ~lock_dirs:lock_dirs_arg
    |> List.concat_map ~f:(fun lock_dir ->
      lock_dir.packages
      |> Package_name.Map.values
      |> List.concat_map ~f:(fun (pkg : Lock_dir.Pkg.t) -> pkg.depexts))
  in
  Console.print [ Pp.concat_map ~sep:Pp.newline ~f:Pp.verbatim depexts ]
;;

let term =
  let+ builder = Common.Builder.term
  and+ lock_dirs_arg = Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () -> print_depexts ~lock_dirs_arg)
;;

let info =
  let doc = "Print the list of all the available depexts" in
  Cmd.info "depexts" ~doc
;;

let command = Cmd.v info term
