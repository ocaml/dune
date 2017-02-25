open Jbuilder
open Import
open Jbuilder_cmdliner.Cmdliner

module Main = Jbuilder.Main

(* Things in src/ don't depend on cmdliner to speed up the bootstrap, so we set this
   reference here *)
let () = suggest_function := Jbuilder_cmdliner.Cmdliner_suggest.value

let (>>=) = Future.(>>=)

let create_log = Main.create_log

type common =
  { concurrency: int
  ; debug_rules: bool
  ; debug_dep_path: bool
  ; debug_findlib: bool
  ; dev_mode: bool
  }

let set_common c =
  Clflags.concurrency := c.concurrency;
  Clflags.debug_rules := c.debug_rules;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.dev_mode := c.dev_mode

let copts_sect = "COMMON OPTIONS"
let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `S "BUGS"
  ; `P "Check bug reports at https://github.com/janestreet/jbuilder/issues"
  ]

let common =
  let make concurrency debug_rules debug_dep_path debug_findlib dev_mode =
    { concurrency
    ; debug_rules
    ; debug_dep_path
    ; debug_findlib
    ; dev_mode
    }
  in
  let docs = copts_sect in
  let concurrency =
    Arg.(value & opt int !Clflags.concurrency & info ["j"] ~docs) in
  let drules = Arg.(value & flag & info ["drules"] ~docs) in
  let ddep_path = Arg.(value & flag & info ["ddep-path"] ~docs) in
  let dfindlib = Arg.(value & flag & info ["dfindlib"] ~docs) in
  let dev = Arg.(value & flag & info ["dev"] ~docs) in
  Term.(const make $ concurrency $ drules $ ddep_path $ dfindlib $ dev)

let installed_libraries =
  let doc = "Print out libraries installed on the system." in
  let go common =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Lazy.force Context.default >>= fun ctx ->
       let findlib = Findlib.create ctx in
       let pkgs = Findlib.all_packages findlib in
       let max_len = List.longest_map pkgs ~f:(fun p -> p.name) in
       List.iter pkgs ~f:(fun pkg ->
         let ver =
           match pkg.Findlib.version with
           | "" -> "n/a"
           | v  -> v
         in
         Printf.printf "%-*s (version: %s)\n" max_len pkg.name ver);
       Future.return ())
  in
  ( Term.(const go
          $ common)
  , Term.info "installed-libraries" ~doc
  )

let resolve_package_install setup pkg =
  match Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    die "Unknown package %s!%s" pkg (hint pkg (String_map.keys setup.packages))

let build_package pkg =
  Future.Scheduler.go ~log:(create_log ())
    (Main.setup () >>= fun setup ->
     Build_system.do_build_exn setup.build_system
       [resolve_package_install setup pkg])

let build_package =
  let doc = "Build a single package in release mode." in
  let name_ = Arg.info [] ~docv:"PACKAGE-NAME" in
  let go common pkg =
    set_common common;
    build_package pkg
  in
  ( Term.(const go
          $ common
          $ Arg.(required & pos 0 (some string) None name_))
  , Term.info "build-package" ~doc ~man:help_secs)

let external_lib_deps packages =
  let log = create_log () in
  let deps =
    Path.Map.fold (Main.external_lib_deps ~log ~packages ()) ~init:String_map.empty
      ~f:(fun ~key:_ ~data:deps acc -> Build.merge_lib_deps deps acc)
  in
  String_map.iter deps ~f:(fun ~key:n ~data ->
    match (data : Build.lib_dep_kind) with
    | Required -> Printf.printf "%s\n" n
    | Optional -> Printf.printf "%s (optional)\n" n)

let external_lib_deps =
  let doc = "Print out external library dependencies." in
  let name_ = Arg.info [] ~docv:"PACKAGE-NAME" in
  let go common packages =
    set_common common;
    external_lib_deps packages
  in
  ( Term.(const go
          $ common
          $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "external-lib-deps" ~doc ~man:help_secs)

type target =
  | File  of Path.t
  | Alias of Path.t * Alias.t

let resolve_targets (setup : Main.setup) user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let targets =
      List.concat_map user_targets ~f:(fun s ->
        if String.is_prefix s ~prefix:"@" then
          let s = String.sub s ~pos:1 ~len:(String.length s - 1) in
          let path = Path.relative Path.root s in
          if Path.is_root path then
            die "@ on the command line must be followed by a valid alias name"
          else
            let dir = Path.parent path in
            let name = Path.basename path in
            [Alias (path, Alias.make ~dir name)]
        else
          let path = Path.relative Path.root s in
          if Path.is_in_build_dir path      ||
             not (Path.is_local path)       ||
             Path.exists path               ||
             Build_system.is_target setup.build_system path then
            [File path]
          else
            match
              List.filter_map setup.contexts ~f:(fun ctx ->
                let path = Path.append ctx.Context.build_dir path in
                if Build_system.is_target setup.build_system path then
                  Some (File path)
                else
                  None)
            with
            | [] -> die "Don't know how to build %s" (Path.to_string path)
            | l  -> l
        )
    in
    Printf.printf "Building the following targets:\n";
    List.iter targets ~f:(function
      | File path ->
        Printf.printf "- %s\n" (Path.to_string path)
      | Alias (path, _) ->
        Printf.printf "- alias %s\n" (Path.to_string path));
    flush stdout;
    List.map targets ~f:(function
      | File path -> path
      | Alias (_, alias) -> Alias.file alias)

let build_targets =
  let doc = "Build targets." in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let go common targets =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup () >>= fun setup ->
       let targets = resolve_targets setup targets in
       Build_system.do_build_exn setup.build_system targets) in
  ( Term.(const go
          $ common
          $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "build" ~doc ~man:help_secs)

let runtest =
  let doc = "Run tests." in
  let name_ = Arg.info [] ~docv:"DIR" in
  let go common dirs =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup () >>= fun setup ->
       let targets =
         List.map dirs ~f:(fun dir ->
           let dir = Path.(relative root) dir in
           Alias.file (Alias.runtest ~dir))
       in
       Build_system.do_build_exn setup.build_system targets) in
  ( Term.(const go
          $ common
          $ Arg.(value & pos_all string ["."] name_))
  , Term.info "runtest" ~doc ~man:help_secs)

let opam_installer () =
  match Bin.which "opam-installer" with
  | None ->
    die "\
Sorry, you need the opam-installer tool to be able to install or
uninstall packages.

I couldn't find the opam-installer binary :-("
  | Some fn -> fn

let get_prefix context ~from_command_line =
  match from_command_line with
  | Some p -> Future.return (Path.of_string p)
  | None -> Context.install_prefix context

let install_uninstall ~what =
  let doc = sprintf "%s packages using opam-installer." (String.capitalize what) in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let go common prefix pkgs =
    set_common common;
    let opam_installer = opam_installer () in
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup () >>= fun setup ->
       let pkgs =
         match pkgs with
         | [] -> String_map.keys setup.packages
         | l  -> l
       in
       let install_files, missing_install_files =
         List.partition_map pkgs ~f:(fun pkg ->
           let fn = resolve_package_install setup pkg in
           if Path.exists fn then
             Inl fn
           else
             Inr pkg)
       in
       if missing_install_files <> [] then begin
         die "The <package>.install files for these packages are missing:\n\
              %s\n\
              You need to run: jbuilder build %s"
           (String.concat ~sep:"\n"
              (List.map missing_install_files ~f:(sprintf "- %s")))
           (String.concat ~sep:" " (List.map pkgs ~f:(sprintf "%s.install")))
       end;
       (match setup.contexts, prefix with
        | _ :: _ :: _, Some _ ->
          die "Cannot specify --prefix when installing into multiple contexts!"
        | _ -> ());
       Future.all_unit
         (List.map setup.contexts ~f:(fun context ->
            get_prefix context ~from_command_line:prefix >>= fun prefix ->
            Future.all_unit
              (List.map install_files ~f:(fun path ->
                   Future.run (Path.to_string opam_installer)
                     [ sprintf "-%c" what.[0]
                     ; "--prefix"
                     ; Path.to_string prefix
                     ; Path.to_string path
                     ])))))
  in
  ( Term.(const go
          $ common
          $ Arg.(value & opt (some dir) None & info ["prefix"])
          $ Arg.(value & pos_all string [] name_))
  , Term.info what ~doc ~man:help_secs)

let install   = install_uninstall ~what:"install"
let uninstall = install_uninstall ~what:"uninstall"

let all =
  [ installed_libraries
  ; build_package
  ; external_lib_deps
  ; build_targets
  ; runtest
  ; install
  ; uninstall
  ]

let default =
  let doc = "fast, portable and opinionated build system for OCaml" in
  ( Term.(ret @@ const @@ `Help (`Pager, None))
  , Term.info "jbuilder" ~doc)

let () =
  Ansi_color.setup_err_formatter_colors ();
  try
    match Term.eval_choice default all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with exn ->
    Format.eprintf "%a@?" (Main.report_error ?map_fname:None) exn;
    exit 1
