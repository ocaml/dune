open Import

let doc = "Write the standard start/dune file."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune start-file) writes a `dune` file containing a standard `start/build`
        alias. By default it writes to `start/dune`, but you can pass a different
        directory path as the optional positional argument.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "start-file" ~doc ~man

let dir_arg =
  let docv = "PATH" in
  let doc = "Directory where the start dune file should be written." in
  Arg.(value & pos 0 string "start/" & info [] ~docv ~doc:(Some doc))
;;

let contents =
  String.concat
    ~sep:"\n"
    [ ";; To develop interactively, you are meant to start dune as:"
    ; ";; $ dune build -w -alias start/build"
    ; ""
    ; "(alias"
    ; " (name build)"
    ; " (deps"
    ; "  (alias_rec %{project_root}/check)"
    ; "  ;; Targets should go here. A few common ones are commented out for your"
      ^ " convenience:"
    ; "  ;; (alias_rec %{project_root}/runtest)"
    ; "  ;; (alias_rec %{project_root}/install)"
    ; "  ;; (alias_rec %{project_root}/ocaml-index)"
    ; "  ))"
    ; ""
    ]
;;

let path_for_message path =
  let path = Path.to_absolute_filename path in
  let path =
    match String.drop_prefix path ~prefix:Fpath.initial_cwd with
    | Some s when (not (String.is_empty s)) && Char.equal s.[0] Filename.dir_sep.[0] ->
      String.drop s 1
    | _ -> path
  in
  String.maybe_quoted path
;;

let print_completion path =
  let open Pp.O in
  Console.print_user_message
    (User_message.make
       [ Pp.tag User_message.Style.Ok (Pp.verbatim "Success")
         ++ Pp.text ": wrote start file to "
         ++ Pp.tag User_message.Style.Kwd (Pp.verbatim (path_for_message path))
       ])
;;

let ensure_directory path =
  let path_string = Path.to_string path in
  if Fpath.exists path_string && not (Fpath.is_directory path_string)
  then User_error.raise [ Pp.textf "%s is not a directory" (path_for_message path) ];
  Path.mkdir_p path
;;

let write path =
  ensure_directory path;
  let target = Path.relative path "dune" in
  let target_string = Path.to_string target in
  if Fpath.exists target_string
  then (
    if Fpath.is_directory target_string
    then
      User_error.raise
        [ Pp.textf "%s already exists and is a directory" (path_for_message target) ];
    let existing = Io.read_file ~binary:false target in
    if existing <> contents
    then
      User_error.raise
        [ Pp.textf "Refusing to overwrite existing file %s" (path_for_message target) ])
  else Io.write_file ~binary:false target contents;
  print_completion target
;;

let term =
  let+ path = dir_arg in
  write (Path.of_filename_relative_to_initial_cwd path)
;;

let command = Cmd.v info term
