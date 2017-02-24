open Jbuilder
open Import
open Jbuilder_cmdliner.Cmdliner

module Main = Jbuilder.Main

(* Things in src/ don't depend on cmdliner to speed up the bootstrap, so we set this
   reference here *)
let () = suggest_function := Jbuilder_cmdliner.Cmdliner_suggest.value

let (>>=) = Future.(>>=)

let create_log = Main.create_log

(* TODO: rewrite this when command trees are supported.

   https://github.com/dbuenzli/cmdliner/issues/24 *)
let internal = function
  | [_; "findlib-packages"] ->
    Future.Scheduler.go ~log:(create_log ())
      (Lazy.force Context.default >>= fun ctx ->
       let findlib = Findlib.create ctx in
       let pkgs = Findlib.all_packages findlib in
       let max_len =
         List.map pkgs ~f:String.length
         |> List.fold_left ~init:0 ~f:max
       in
       List.iter pkgs ~f:(fun pkg ->
         let ver =
           match (Findlib.find_exn findlib pkg).version with
           | "" -> "n/a"
           | v  -> v
         in
         Printf.printf "%-*s (version: %s)\n" max_len pkg ver);
       Future.return ())
  | _ ->
    ()

let internal =
  let doc = "internal use only" in
  let name_ = Arg.info [] in
  ( Term.(const internal $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "internal" ~doc)

type common =
  { concurrency: int
  ; debug_rules: bool
  ; debug_dep_path: bool
  ; debug_findlib: bool
  }

let set_common c =
  Clflags.concurrency := c.concurrency;
  Clflags.debug_rules := c.debug_rules;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib

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
  let make concurrency debug_rules debug_dep_path debug_findlib =
    { concurrency ; debug_rules ; debug_dep_path ; debug_findlib } in
  let docs = copts_sect in
  let concurrency =
    Arg.(value & opt int !Clflags.concurrency & info ["j"] ~docs) in
  let drules = Arg.(value & flag & info ["drules"] ~docs) in
  let ddep_path = Arg.(value & flag & info ["ddep-path"] ~docs) in
  let dfindlib = Arg.(value & flag & info ["dfindlib"] ~docs) in
  Term.(const make $ concurrency $ drules $ ddep_path $ dfindlib)

let build_package pkg =
  Future.Scheduler.go ~log:(create_log ())
    (Main.setup () >>= fun setup ->
     match Main.package_install_file setup pkg with
     | Ok path ->
       Build_system.do_build_exn setup.build_system
         [path]
     | Error () ->
       die "Unknown package %s!%s" pkg (hint pkg (String_map.keys setup.packages))
    )

let build_package =
  let doc = "build a package in release mode" in
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
    Path.Map.fold (Main.external_lib_deps ~log ~packages) ~init:String_map.empty
      ~f:(fun ~key:_ ~data:deps acc -> Build.merge_lib_deps deps acc)
  in
  String_map.iter deps ~f:(fun ~key:n ~data ->
    match (data : Build.lib_dep_kind) with
    | Required -> Printf.printf "%s\n" n
    | Optional -> Printf.printf "%s (optional)\n" n)

let external_lib_deps =
  let doc = "print out external library dependencies" in
  let name_ = Arg.info [] ~docv:"PACKAGE-NAME" in
  let go common packages =
    set_common common;
    external_lib_deps packages
  in
  ( Term.(const go
          $ common
          $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "external-lib-deps" ~doc ~man:help_secs)

let resolve_targets (setup : Main.setup)user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let user_targets = List.map user_targets ~f:(Path.relative Path.root) in
    let real_targets =
      List.map user_targets ~f:(fun path ->
        if Path.is_in_build_dir path then
          path
        else if Path.is_local path &&
                not (Build_system.is_target setup.build_system path) &&
                not (Path.exists path) then
          Path.append setup.context.build_dir path
        else
          path)
    in
    Printf.printf "Building the following targets:\n";
    List.iter real_targets ~f:(fun target ->
      Printf.printf "- %s\n" (Path.to_string target));
    flush stdout;
    real_targets

let build_targets ~name =
  let doc = "build targets" in
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
  , Term.info name ~doc ~man:help_secs)

let runtest =
  let doc = "run tests" in
  let name_ = Arg.info [] ~docv:"DIR" in
  let go common dirs =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup () >>= fun setup ->
       let dirs =
         match dirs with
         | [] -> [Path.root]
         | _  -> List.map dirs ~f:Path.(relative root)
       in
       let targets =
         List.map dirs ~f:(fun dir ->
           let dir =
             if Path.is_in_build_dir dir then
               dir
             else
               Path.append setup.context.build_dir dir
           in
           Alias.file (Alias.runtest ~dir))
       in
       Build_system.do_build_exn setup.build_system targets) in
  ( Term.(const go
          $ common
          $ Arg.(value & pos_all string [] name_))
  , Term.info "runtest" ~doc ~man:help_secs)

let all =
  [ internal
  ; build_package
  ; external_lib_deps
  ; build_targets ~name:"build"
  ; runtest
  ]

let () =
  Ansi_color.setup_err_formatter_colors ();
  try
    match Term.eval_choice (build_targets ~name:"jbuilder") all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with exn ->
    Format.eprintf "%a@?" (Main.report_error ?map_fname:None) exn;
    exit 1
