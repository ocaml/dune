open Import
module Lock_dir = Dune_pkg.Lock_dir

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
        [ Pp.hbox
          @@ Pp.textf "Contents of %s:" (Path.Source.to_string_maybe_quoted lock_dir_path)
        ; Pkg.Lock.pp_packages lock_dir.packages
        ])
  ;;

  let command =
    let doc = "Display packages in a lock file" in
    let info = Cmd.info ~doc "lock" in
    Cmd.v info term
  ;;
end

let command =
  let doc = "Subcommands related to package management" in
  let info = Cmd.info ~doc "pkg" in
  Cmd.group info [ Lock.command ]
;;
