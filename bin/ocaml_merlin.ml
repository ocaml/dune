open! Stdune
open Import

module Server : sig
  val dump : string -> unit Fiber.t

  val dump_dot_merlin : string -> unit Fiber.t

  (** Once started the server will wait for commands on stdin, read the
      requested merlin dot file and return its content on stdout. The server
      will halt when receiving EOF of a bad csexp. *)
  val start : unit -> unit Fiber.t
end = struct
  open! Stdune
  open Fiber.O

  module Merlin_conf = struct
    type t = Sexp.t

    let make_error msg = Sexp.(List [ List [ Atom "ERROR"; Atom msg ] ])

    let to_stdout (t : t) =
      Csexp.to_channel stdout t;
      flush stdout
  end

  module Commands = struct
    type t =
      | File of string
      | Halt
      | Unknown of string

    let read_input in_channel =
      match Csexp.input in_channel with
      | Ok sexp -> (
        let open Sexp in
        match sexp with
        | Atom "Halt" -> Halt
        | List [ Atom "File"; Atom path ] -> File path
        | sexp ->
          let msg = Printf.sprintf "Bad input: %s" (Sexp.to_string sexp) in
          Unknown msg)
      | Error err ->
        Format.eprintf "Bad input: %s@." err;
        Halt
  end

  (* [make_relative_to_root p] will check that [Path.root] is a prefix of the
     absolute path [p] and remove it if that is the case. Under Windows and
     Cygwin environment both paths are lowarcased before the comparison *)
  let make_relative_to_root p =
    let prefix = Path.(to_absolute_filename root) in
    let p = Path.(to_absolute_filename p) in
    let prefix, p =
      if Sys.win32 || Sys.cygwin then
        (String.lowercase_ascii prefix, String.lowercase_ascii p)
      else (prefix, p)
    in
    String.drop_prefix ~prefix p
    (* After dropping the prefix we need to remove the leading path separator *)
    |> Option.map ~f:(fun s -> String.drop s 1)

  (* [to_local p] makes path [p] relative to the project's root. [p] can be: -
     An absolute path - A path relative to [Path.initial_cwd] *)
  let to_local file_path =
    let error msg = Error msg in
    (* This ensure the path is absolute. If not it is prefixed with
       [Path.initial_cwd] *)
    let abs_file_path = Path.of_filename_relative_to_initial_cwd file_path in
    (* Then we make the path relative to [Path.root] (and not
       [Path.initial_cwd]) *)
    match make_relative_to_root abs_file_path with
    | Some path -> (
      try
        let path = Path.of_string path in
        (* If dune ocaml-merlin is called from within the build dir we must
           remove the build context *)
        Ok (Path.drop_optional_build_context path |> Path.local_part)
      with User_error.E mess -> User_message.to_string mess |> error)
    | None ->
      Printf.sprintf "Path %S is not in dune workspace (%S)." file_path
        Path.(to_absolute_filename Path.root)
      |> error

  (* Given a path [p] relative to the workspace root, [get_merlin_files_paths p]
     navigates to the [_build] directory and reaches this path from the correct
     context. Then it returns the list of available Merlin configurations for
     this directory. *)
  let get_merlin_files_paths local_path =
    let module Context_name = Dune_engine.Context_name in
    let+ workspace = Memo.run (Workspace.workspace ()) in
    let merlin_path =
      let ctx_root =
        let context =
          Option.value ~default:Context_name.default workspace.merlin_context
        in
        let ctx = Context_name.to_string context in
        Path.Build.(relative root ctx)
      in
      let dir_path = Path.Build.(append_local ctx_root local_path) in
      Path.Build.relative dir_path Dune_rules.Merlin_ident.merlin_folder_name
    in
    Path.build merlin_path |> Path.readdir_unsorted |> Result.value ~default:[]
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun f -> Path.Build.relative merlin_path f |> Path.build)

  module Merlin = Dune_rules.Merlin

  let load_merlin_file local_path file =
    (* We search for an appropriate merlin configuration in the current
       directory and its parents *)
    let rec find_closest path =
      let filename = String.lowercase_ascii file in
      let* file_paths = get_merlin_files_paths path in
      let result =
        List.find_map file_paths ~f:(fun file_path ->
            if Path.exists file_path then
              match Merlin.Processed.load_file file_path with
              | Ok config -> Merlin.Processed.get config ~filename
              | Error msg -> Some (Merlin_conf.make_error msg)
            else None)
      in
      match result with
      | Some p -> Fiber.return (Some p)
      | None -> (
        match
          if Path.Local.is_root path then None else Path.Local.parent path
        with
        | None -> Fiber.return None
        | Some dir -> find_closest dir)
    in
    let default =
      Printf.sprintf
        "No config found for file %S in %S. Try calling `dune build`." file
        (Path.Local.to_string local_path)
      |> Merlin_conf.make_error
    in
    find_closest local_path >>| function
    | None -> default
    | Some x -> x

  let print_merlin_conf file =
    let dir, file = Filename.(dirname file, basename file) in
    let+ answer =
      match to_local dir with
      | Ok p -> load_merlin_file p file
      | Error s -> Fiber.return (Merlin_conf.make_error s)
    in
    Merlin_conf.to_stdout answer

  let dump s =
    match to_local s with
    | Ok path ->
      get_merlin_files_paths path >>| List.iter ~f:Merlin.Processed.print_file
    | Error mess ->
      Printf.eprintf "%s\n%!" mess;
      Fiber.return ()

  let dump_dot_merlin s =
    match to_local s with
    | Ok path ->
      let+ files = get_merlin_files_paths path in
      Merlin.Processed.print_generic_dot_merlin files
    | Error mess ->
      Printf.eprintf "%s\n%!" mess;
      Fiber.return ()

  let start () =
    let rec main () =
      match Commands.read_input stdin with
      | File path ->
        let* () = print_merlin_conf path in
        main ()
      | Unknown msg ->
        Merlin_conf.to_stdout (Merlin_conf.make_error msg);
        main ()
      | Halt -> Fiber.return ()
    in
    main ()
end

let doc = "Start a merlin configuration server"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune ocaml-merlin) starts a server that can be queried to get
      .merlin information. It is meant to be used by Merlin itself and does not
      provide a user-friendly output.|}
  ; `Blocks Common.help_secs
  ; Common.footer
  ]

let info = Term.info "ocaml-merlin" ~doc ~man

let term =
  let+ common = Common.term
  and+ dump_config =
    Arg.(
      value
      & opt ~vopt:(Some ".") (some string) None
      & info [ "dump-config" ]
          ~doc:
            "Prints the entire content of the merlin configuration for the \
             given folder in a user friendly form. This is for testing and \
             debugging purposes only and should not be considered as a stable \
             output.")
  in
  let common = Common.set_print_directory common false in
  let config = Common.init common ~log_file:No_log_file in
  Scheduler.go ~common ~config (fun () ->
      match dump_config with
      | Some s -> Server.dump s
      | None -> Server.start ())

let command = (term, info)

module Dump_dot_merlin = struct
  let doc = "Print Merlin configuration"

  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|$(b,dune ocaml dump-dot-merlin) will attempt to read previously
        generated configuration in a source folder, merge them and print
        it to the standard output in Merlin configuration syntax. The
        output of this command should always be checked and adapted to
        the project needs afterward.|}
    ; Common.footer
    ]

  let info = Term.info "dump-dot-merlin" ~doc ~man

  let term =
    let+ common = Common.term
    and+ path =
      Arg.(
        value
        & pos 0 (some string) None
        & info [] ~docv:"PATH"
            ~doc:
              "The path to the folder of which the configuration should be \
               printed. Defaults to the current directory.")
    in
    let config = Common.init common ~log_file:No_log_file in
    Scheduler.go ~common ~config (fun () ->
        match path with
        | Some s -> Server.dump_dot_merlin s
        | None -> Server.dump_dot_merlin ".")

  let command = (term, info)
end
