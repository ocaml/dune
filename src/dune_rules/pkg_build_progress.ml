open! Import

let enabled = Config.make_toggle ~name:"pkg_build_progress" ~default:`Disabled

let display package_name package_version status =
  let status_tag = User_message.Style.Ok in
  let status_text =
    match status with
    | `Downloading -> "Downloading"
    | `Building -> "Building"
  in
  let user_message =
    User_message.make
      [ Pp.concat
          [ Pp.tag status_tag (Pp.text status_text)
          ; Pp.textf
              " %s.%s"
              (Package.Name.to_string package_name)
              (Package_version.to_string package_version)
          ]
      ]
  in
  Console.print_user_message user_message
;;

let display_build_progress package_name package_version ~source_dir ~target_dir =
  let open Action_builder.O in
  let source =
    match source_dir with
    | `No_package_source -> Action_builder.return ()
    | `Some source_dir ->
      let* () = Action_builder.return () in
      (* XXX Ideally we would use [Fs.dir_exists] here, but this
         function always returns true when the path in question is
         inside the _build directory. *)
      if Path.Untracked.exists (Path.build source_dir)
      then Action_builder.return ()
      else (
        display package_name package_version `Downloading;
        Action_builder.path (Path.build source_dir))
  in
  let target =
    match target_dir with
    | `No_build_or_install_command -> Action_builder.return ()
    | `Some target_dir ->
      let+ () = Action_builder.return () in
      if not (Path.Untracked.exists (Path.build target_dir))
      then display package_name package_version `Building
  in
  match Config.get enabled with
  | `Enabled ->
    let* () = source in
    target
  | `Disabled -> Action_builder.return ()
;;
