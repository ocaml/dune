open Import

type command =
  { prog : string
  ; argv : string list
  }

let prog () =
  match Platform.OS.value with
  | Linux -> Util.find_in_path_exn "bwrap"
  | _ ->
    User_error.raise [ Pp.text "--sandbox-actions is currently only supported on Linux" ]
;;

let shared_cache_bindings () =
  let build_cache_dir = Lazy.force Dune_cache.Layout.build_cache_dir in
  Path.mkdir_p build_cache_dir;
  let build_cache_dir = Path.to_string build_cache_dir in
  [ "--ro-bind"; build_cache_dir; build_cache_dir ]
;;

let wrap ~cwd argv =
  let prog = Path.to_string (prog ()) in
  { prog
  ; argv =
      [ prog; "--die-with-parent"; "--bind"; "/"; "/" ]
      @ shared_cache_bindings ()
      @ [ "--proc"; "/proc"; "--dev"; "/dev"; "--chdir"; cwd; "--" ]
      @ argv
  }
;;

module With_bwrap = struct
  let command =
    let doc = "Run a command under Dune's bubblewrap wrapper." in
    let info = Cmd.info "with-bwrap" ~doc in
    let term =
      let+ argv = Arg.(value & pos_all string [] (info [] ~docv:"COMMAND" ~doc:None)) in
      match argv with
      | [] -> User_error.raise [ Pp.text "missing command after --" ]
      | _ :: _ ->
        let { prog; argv } = wrap ~cwd:(Sys.getcwd ()) argv in
        (match argv with
         | _ :: args -> Proc.restore_cwd_and_execve prog args ~env:Env.initial
         | [] -> Code_error.raise "bwrap command is empty" [])
    in
    Cmd.v info term
  ;;
end
