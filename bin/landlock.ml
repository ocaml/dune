open Import

let resolve_prog prog =
  if Fpath.contains_path_sep prog
  then prog
  else Path.to_string (Util.find_in_path_exn prog)
;;

module With_landlock = struct
  let command =
    let doc = "Run a command under Dune's Landlock wrapper." in
    let info = Cmd.info "with-landlock" ~doc in
    let term =
      let+ write_dirs =
        Arg.(
          value
          & opt_all string []
          & info
              [ "write-dir" ]
              ~docv:"DIR"
              ~doc:(Some "Allow writes under this directory."))
      and+ argv = Arg.(value & pos_all string [] (info [] ~docv:"COMMAND" ~doc:None)) in
      match argv with
      | [] -> User_error.raise [ Pp.text "missing command after --" ]
      | prog :: args ->
        if not (Spawn.Landlock.available ())
        then User_error.raise [ Pp.text "Landlock is not available on this system" ];
        let fds =
          List.map write_dirs ~f:(fun dir ->
            let dir = Path.of_filename_relative_to_initial_cwd dir in
            Path.mkdir_p dir;
            Unix.openfile (Path.to_string dir) [ O_RDONLY; O_CLOEXEC ] 0
            |> Fd.unsafe_of_unix_file_descr)
        in
        Exn.protect
          ~finally:(fun () -> List.iter fds ~f:Fd.close)
          ~f:(fun () ->
            Spawn.Landlock.allow_writes_to_directories fds |> Spawn.Landlock.restrict);
        Proc.restore_cwd_and_execve (resolve_prog prog) args ~env:Env.initial
    in
    Cmd.v info term
  ;;
end
