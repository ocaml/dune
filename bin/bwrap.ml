open Import

type command =
  { prog : string
  ; argv : string list
  }

let find_in_path_exn prog =
  match Bin.which ~path:(Env_path.path Env.initial) prog with
  | Some path -> path
  | None -> User_error.raise [ Pp.textf "unable to find %s in PATH" prog ]
;;

let bwrap_prog () =
  match Platform.OS.value with
  | Linux -> find_in_path_exn "bwrap"
  | _ ->
    User_error.raise [ Pp.text "Dune's bubblewrap wrapper is only supported on Linux" ]
;;

let shared_cache_dir () =
  let build_cache_dir = Lazy.force Dune_cache.Layout.build_cache_dir in
  Path.mkdir_p build_cache_dir;
  Path.to_string build_cache_dir
;;

let shared_cache_bindings () =
  let build_cache_dir = shared_cache_dir () in
  [ "--ro-bind"; build_cache_dir; build_cache_dir ]
;;

let wrap_with_bwrap ~cwd argv =
  let prog = Path.to_string (bwrap_prog ()) in
  { prog
  ; argv =
      [ prog; "--die-with-parent"; "--bind"; "/"; "/" ]
      @ shared_cache_bindings ()
      @ [ "--proc"; "/proc"; "--dev"; "/dev"; "--chdir"; cwd; "--" ]
      @ argv
  }
;;

let wrap_with_sandbox_exec =
  (* Mostly for tests: cram workspaces often live under macOS paths that are
     reachable through both [/tmp] and [/private/tmp], or through both [/var] and
     [/private/var]. *)
  let add_macos_symlink_aliases path =
    let add_alias ~prefix ~alias paths =
      match String.drop_prefix path ~prefix with
      | None -> paths
      | Some rest -> (alias ^ rest) :: paths
    in
    [ path ]
    |> add_alias ~prefix:"/private/var/" ~alias:"/var/"
    |> add_alias ~prefix:"/var/" ~alias:"/private/var/"
    |> add_alias ~prefix:"/private/tmp/" ~alias:"/tmp/"
    |> add_alias ~prefix:"/tmp/" ~alias:"/private/tmp/"
  in
  let sandbox_exec_profile () =
    let sbpl_string s =
      let buf = Buffer.create (String.length s + 2) in
      Buffer.add_char buf '"';
      String.iter s ~f:(function
        | '"' -> Buffer.add_string buf "\\\""
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\t' -> Buffer.add_string buf "\\t"
        | c -> Buffer.add_char buf c);
      Buffer.add_char buf '"';
      Buffer.contents buf
    in
    let protected_paths =
      let build_cache_dir = shared_cache_dir () in
      [ build_cache_dir; Unix.realpath build_cache_dir ]
      |> List.concat_map ~f:add_macos_symlink_aliases
      |> List.sort_uniq ~compare:String.compare
    in
    let deny_write path =
      let path = sbpl_string path in
      sprintf "(deny file-write* (literal %s) (subpath %s))" path path
    in
    String.concat
      ~sep:"\n"
      ([ "(version 1)"; "(allow default)" ] @ List.map protected_paths ~f:deny_write)
  in
  fun ~cwd argv ->
    let prog =
      Path.to_string
        (match Platform.OS.value with
         | Darwin -> find_in_path_exn "sandbox-exec"
         | _ ->
           User_error.raise
             [ Pp.text "Dune's sandbox-exec wrapper is only supported on macOS" ])
    in
    { prog
    ; argv =
        [ prog
        ; "-p"
        ; sandbox_exec_profile ()
        ; "/bin/sh"
        ; "-c"
        ; "cd \"$1\" || exit $?; shift; exec \"$@\""
        ; "dune-sandbox-exec"
        ; cwd
        ]
        @ argv
    }
;;

let wrap ~cwd argv =
  match Platform.OS.value with
  | Linux -> wrap_with_bwrap ~cwd argv
  | Darwin -> wrap_with_sandbox_exec ~cwd argv
  | _ ->
    User_error.raise [ Pp.text "--sandbox-actions is only supported on Linux and macOS" ]
;;

let command ~name ~doc ~wrap ~empty_error =
  let info = Cmd.info name ~doc in
  let term =
    let+ argv = Arg.(value & pos_all string [] (info [] ~docv:"COMMAND" ~doc:None)) in
    match argv with
    | [] -> User_error.raise [ Pp.text "missing command after --" ]
    | _ :: _ ->
      let { prog; argv } = wrap ~cwd:(Sys.getcwd ()) argv in
      (match argv with
       | _ :: args -> Proc.restore_cwd_and_execve prog args ~env:Env.initial
       | [] -> Code_error.raise empty_error [])
  in
  Cmd.v info term
;;

module With_bwrap = struct
  let command =
    command
      ~name:"with-bwrap"
      ~doc:"Run a command under Dune's bubblewrap wrapper."
      ~wrap:wrap_with_bwrap
      ~empty_error:"bwrap command is empty"
  ;;
end

module With_sandbox_exec = struct
  let command =
    command
      ~name:"with-sandbox-exec"
      ~doc:"Run a command under Dune's sandbox-exec wrapper."
      ~wrap:wrap_with_sandbox_exec
      ~empty_error:"sandbox-exec command is empty"
  ;;
end
