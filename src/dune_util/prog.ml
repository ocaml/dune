open Import

let system_shell_exn =
  let cmd, arg, os = if Sys.win32 then "cmd", "/c", " on Windows" else "sh", "-c", "" in
  let bin = lazy (Bin.which ~path:(Env_path.path Env.initial) cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
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
