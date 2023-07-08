open Import

let lookup_os_shell_path' ?(env = Env.initial) shell_variant =
  let path = Env_path.path env in
  let shell_prog_name =
    match shell_variant with
    | `system when Sys.win32 -> "cmd"
    | `system -> "sh"
    | `bash -> "bash"
  in
  let shell =
    match Bin.which ~path shell_prog_name with
    | None -> Bin.which ~path:(Env_path.path Env.initial) shell_prog_name
    | Some _ as s -> s
  in
  (shell_prog_name, shell)

let lookup_os_shell_path ?env shell = snd (lookup_os_shell_path' ?env shell)

let system_shell_exn =
  let arg, os = if Sys.win32 then ("/c", "on Windows") else ("-c", "") in
  let vanilla_bin = lazy (lookup_os_shell_path' `system) in
  fun ?env ~needed_to () ->
    let cmd, bin =
      match env with
      | None -> Lazy.force vanilla_bin
      | Some env -> lookup_os_shell_path' ~env `system
    in
    match bin with
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
