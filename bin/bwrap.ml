open Import

type command =
  { prog : string
  ; argv : string list
  }

type availability =
  | Available
  | Not_found
  | Probe_failed of string

let process_status_to_string = function
  | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
  | WSIGNALED n -> Printf.sprintf "signal %d" n
  | WSTOPPED n -> Printf.sprintf "stopped by signal %d" n
;;

let probe prog =
  let prog = Path.to_string prog in
  let argv = [| prog; "--bind"; "/"; "/"; "--"; "true" |] in
  let env = Env.to_unix Env.initial |> Array.of_list in
  let dev_null = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
  Exn.protect
    ~f:(fun () ->
      match Unix.create_process_env prog argv env dev_null dev_null dev_null with
      | exception Unix.Unix_error (err, _, _) -> Probe_failed (Unix.error_message err)
      | pid ->
        (match snd (Unix.waitpid [] pid) with
         | WEXITED 0 -> Available
         | status -> Probe_failed (process_status_to_string status)))
    ~finally:(fun () -> Unix.close dev_null)
;;

let availability =
  lazy
    (match Platform.OS.value with
     | Linux ->
       (match Bin.which ~path:(Env_path.path Env.initial) "bwrap" with
        | Some prog -> probe prog
        | None -> Not_found)
     | _ -> Not_found)
;;

let available () =
  match Lazy.force availability with
  | Available -> true
  | Not_found | Probe_failed _ -> false
;;

let unavailable_reason () =
  match Platform.OS.value with
  | Linux ->
    (match Lazy.force availability with
     | Available -> []
     | Not_found ->
       [ Pp.text
           "bwrap is unavailable: install the bubblewrap package and ensure the binary \
            is on PATH (Linux only)"
       ]
     | Probe_failed reason ->
       [ Pp.textf
           "bwrap is unavailable: the binary was found but failed to create a sandbox \
            (%s). User namespaces may be disabled."
           reason
       ])
  | _ ->
    [ Pp.text
        "bwrap is unavailable: install the bubblewrap package and ensure the binary is \
         on PATH (Linux only)"
    ]
;;

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
