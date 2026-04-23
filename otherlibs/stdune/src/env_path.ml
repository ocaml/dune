let var = "PATH"

let cons ?(var = var) env ~dir =
  Env.update env ~var ~f:(fun _PATH -> Some (Bin.cons_path dir ~_PATH))
;;

(* [cons_multi env ~dirs] adds each path in [dirs] to the start of the PATH
   variable in [env], preserving their order *)
let cons_multi env ~dirs =
  Env.update env ~var ~f:(fun init ->
    List.fold_right dirs ~init ~f:(fun dir acc -> Some (Bin.cons_path dir ~_PATH:acc)))
;;

let path env =
  match Env.get env var with
  | None -> []
  | Some s -> Bin.parse_path s
;;

let extend_env_concat_path a b =
  let a_including_b's_path = cons_multi a ~dirs:(path b) in
  let b_without_path = Env.remove b ~var in
  Env.extend_env a_including_b's_path b_without_path
;;

let system_shell_exn =
  let cmd, arg, os = if Sys.win32 then "cmd", "/c", " on Windows" else "sh", "-c", "" in
  let bin = lazy (Bin.which ~path:(path Env.initial) cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path when Sys.win32 ->
      (* cmd.exe has a quirky property where it will scan all of its args for
         /c when parsing its flags. Even though CreateProcessW accepts forward
         slash paths, calling C:\Windows\system32/cmd.exe with mixed path
         separators will cause issues because cmd.exe will believe /cmd.exe is
         an argument and fail. In order to avoid this we explicitly replace the
         forward slashes in the cmd.exe path with backslashes. Other programs
         don't have this issue luckily. *)
      Path.of_string (String.replace_char ~from:'/' ~to_:'\\' (Path.to_string path)), arg
    | Some path -> path, arg
    | None ->
      User_error.raise
        [ Pp.textf
            "I need %s to %s but I couldn't find it :(\nWho doesn't have %s%s?!"
            cmd
            needed_to
            cmd
            os
        ]
;;
