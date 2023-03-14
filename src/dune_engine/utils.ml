open Import

let system_shell_exn =
  let cmd, arg, os =
    if Sys.win32 then ("cmd", "/c", "on Windows") else ("sh", "-c", "")
  in
  let bin = lazy (Bin.which ~path:(Env_path.path Env.initial) cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg)
    | None ->
      User_error.raise
        [ Pp.textf
            "I need %s to %s but I couldn't find it :(\nWho doesn't have %s%s?!"
            cmd needed_to cmd os
        ]

let not_found fmt ?loc ?context ?hint x =
  User_error.make ?loc
    (Pp.textf fmt (String.maybe_quoted x)
    ::
    (match context with
    | None -> []
    | Some name -> [ Pp.textf " (context: %s)" (Context_name.to_string name) ])
    )
    ~hints:
      (match hint with
      | None -> []
      | Some hint -> [ Pp.text hint ])

let program_not_found_message ?context ?hint ~loc prog =
  not_found "Program %s not found in the tree or in PATH" ?context ?hint ?loc
    prog

let program_not_found ?context ?hint ~loc prog =
  raise (User_error.E (program_not_found_message ?context ?hint ~loc prog))

let pp_command_hint command =
  let open Pp.O in
  Pp.textf "try:" ++ Pp.newline ++ Pp.cut
  ++ Pp.hbox (Pp.textf "  " ++ Pp.verbatim command)
