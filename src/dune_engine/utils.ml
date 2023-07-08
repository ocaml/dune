open Import

let lookup_os_shell_path' ?(env = Env.initial) ?(cmd_on_windows = true)
    shell_variant =
  let open Option.O in
  let envar_key =
    match shell_variant with
    | `system -> "DUNE_ACTION_SYSTEM_SHELL"
    | `bash -> "DUNE_ACTION_BASH_SHELL"
  in
  let which progn =
    let path = Env_path.path env in
    match Bin.which ~path progn with
    | None -> Bin.which ~path:(Env_path.path Env.initial) progn
    | Some _ as s -> s
  in
  let candidate =
    let classify p =
      let module Filename = Stdlib.Filename in
      if Filename.basename p = p then `program_name
      else if Filename.is_relative p then `relative_path
      else `absolute_path
    in
    Env.get env envar_key >>= fun v ->
    if String.is_empty v then None
    else
      match classify v with
      | `absolute_path ->
        let path = Path.external_ (Path.External.of_string v) in
        if Bin.exists path then Some (Filename.basename v, path)
        else
          User_error.raise
            [ Pp.textf "%s set to path which does not exist: " envar_key
            ; Pp.verbatim v
            ]
      | `program_name -> which v >>| fun p -> (v, p)
      | `relative_path ->
        User_error.raise
          [ Pp.textf "%s cannot be a relative path: " envar_key; Pp.verbatim v ]
  in
  match candidate with
  | Some (progn, shell) -> (progn, Some shell)
  | None ->
    let progn =
      match shell_variant with
      | `system when cmd_on_windows && Sys.win32 -> "cmd"
      | `system -> "sh"
      | `bash -> "bash"
    in
    (progn, which progn)

let lookup_os_shell_path ?env ?cmd_on_windows shell =
  snd (lookup_os_shell_path' ?env ?cmd_on_windows shell)

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
