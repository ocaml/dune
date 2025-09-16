open Import

(* Initialize Path module and console backend *)
let () =
  (* Find project root by looking for dune-project *)
  let rec find_up dir =
    let dune_project = Filename.concat dir "dune-project" in
    if Sys.file_exists dune_project
    then dir
    else (
      let parent = Filename.dirname dir in
      if String.equal parent dir
      then
        User_error.raise
          [ Pp.textf "Could not find workspace root (no dune-project found)" ]
      else find_up parent)
  in
  (* Find workspace root and change to it before setting Path.root *)
  let workspace_root = find_up (Sys.getcwd ()) in
  if Sys.getcwd () <> workspace_root then Sys.chdir workspace_root;
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
  Dune_console.Backend.set Dune_console.Backend.progress
;;

let () =
  let open Cmdliner in
  [ Fetch.fetch_command
  ; Patch.patch_command
  ; Patch.lint_command
  ; Status.outdated_command
  ; Status.list_command
  ]
  |> Cmd.group (Cmd.info "vendor_updater" ~doc:"Fetch vendored packages for Dune")
  |> Cmd.eval
  |> exit
;;
