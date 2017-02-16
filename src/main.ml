open Import
open Future

let internal = function
  | [_; "findlib-packages"] ->
    Future.Scheduler.go
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
       return ())
  | _ ->
    ()

let setup ?filter_out_optional_stanzas_with_missing_deps () =
  let { Jbuild_load. file_tree; tree; stanzas; packages } = Jbuild_load.load () in
  Lazy.force Context.default >>= fun ctx ->
  let rules =
    Gen_rules.gen ~context:ctx ~file_tree ~tree ~stanzas ~packages
      ?filter_out_optional_stanzas_with_missing_deps ()
  in
  let bs = Build_system.create ~file_tree ~rules in
  return (bs, stanzas, ctx)

let external_lib_deps ~packages =
  Future.Scheduler.go
    (setup () ~filter_out_optional_stanzas_with_missing_deps:false
     >>| fun (bs, stanzas, _) ->
     Path.Map.map
       (Build_system.all_lib_deps bs
          (List.map packages ~f:(fun pkg ->
             Path.(relative root) (pkg ^ ".install"))))
       ~f:(fun deps ->
         let internals = Jbuild_types.Stanza.lib_names stanzas in
         String_map.filter deps ~f:(fun name _ -> not (String_set.mem name internals))))

let external_lib_deps_cmd packages =
  let deps =
    Path.Map.fold (external_lib_deps ~packages) ~init:String_map.empty
      ~f:(fun ~key:_ ~data:deps acc -> Build.merge_lib_deps deps acc)
  in
  String_map.iter deps ~f:(fun ~key:n ~data ->
    match (data : Build.lib_dep_kind) with
    | Required -> Printf.printf "%s\n" n
    | Optional -> Printf.printf "%s (optional)\n" n)

let build_package pkg =
  Future.Scheduler.go
    (setup () >>= fun (bs, _, _) ->
     Build_system.do_build_exn bs [Path.(relative root) (pkg ^ ".install")])

module Cli = struct
  open Cmdliner

  let internal =
    let doc = "internal" in
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
    ;`Noblank
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

  let build_package =
    let doc = "build-package" in
    let name_ = Arg.info [] in
    let go common pkg =
      set_common common;
      build_package pkg in
    ( Term.(const go
            $ common
            $ Arg.(required & pos 0 (some string) None name_))
    , Term.info "build-package" ~doc ~man:help_secs)

  let external_lib_deps =
    let doc = "external-lib-deps" in
    let name_ = Arg.info [] in
    let go common packages =
      set_common common;
      external_lib_deps_cmd packages in
    ( Term.(const go
            $ common
            $ Arg.(non_empty & pos_all string [] name_))
    , Term.info "external-lib-deps" ~doc ~man:help_secs)

  let build_targets =
    let doc = "build" in
    let name_ = Arg.info [] in
    let go common targets =
      set_common common;
      Future.Scheduler.go
        (setup () >>= fun (bs, _, ctx) ->
         let targets = List.map targets ~f:(Path.relative ctx.build_dir) in
         Build_system.do_build_exn bs targets) in
    ( Term.(const go
            $ common
            $ Arg.(non_empty & pos_all string [] name_))
    , Term.info "build" ~doc ~man:help_secs)

  let all =
    [ internal ; build_package ; external_lib_deps ; build_targets ]

  let main () =
    match Term.eval_choice build_targets all with
    | `Error _ -> exit 1
    | _ -> exit 0
end

let main = Cli.main

let report_error ?(map_fname=fun x->x) ppf exn ~backtrace =
  match exn with
  | Loc.Error ({ start; stop }, msg) ->
    let start_c = start.pos_cnum - start.pos_bol in
    let stop_c  = stop.pos_cnum  - start.pos_bol in
    Format.fprintf ppf
      "File \"%s\", line %d, characters %d-%d:\n\
       Error: %s\n"
      (map_fname start.pos_fname) start.pos_lnum start_c stop_c msg
  | Fatal_error msg ->
    Format.fprintf ppf "%s\n" (String.capitalize msg)
  | Findlib.Package_not_found pkg ->
    Format.fprintf ppf "Findlib package %S not found.\n" pkg
  | Code_error msg ->
    let bt = Printexc.raw_backtrace_to_string backtrace in
    Format.fprintf ppf "Internal error, please report upstream.\n\
                        Description: %s\n\
                        Backtrace:\n\
                        %s" msg bt
  | _ ->
    let s = Printexc.to_string exn in
    let bt = Printexc.raw_backtrace_to_string backtrace in
    if String.is_prefix s ~prefix:"File \"" then
      Format.fprintf ppf "%s\nBacktrace:\n%s" s bt
    else
      Format.fprintf ppf "Error: exception %s\nBacktrace:\n%s" s bt

let report_error ?map_fname ppf exn =
  match exn with
  | Build_system.Build_error.E err ->
    let module E = Build_system.Build_error in
    report_error ?map_fname ppf (E.exn err) ~backtrace:(E.backtrace err);
    if !Clflags.debug_dep_path then
      Format.fprintf ppf "Dependency path:\n    %s\n"
        (String.concat ~sep:"\n--> "
           (List.map (E.dependency_path err) ~f:Path.to_string))
  | exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    report_error ?map_fname ppf exn ~backtrace

let main () =
  try
    main ()
  with exn ->
    Format.eprintf "%a@?" (report_error ?map_fname:None) exn;
    exit 1
