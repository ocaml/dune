open Import
open Future

let common_args =
  [ "-j", Arg.Set_int Clflags.concurrency, "JOBS concurrency"
  ; "-drules", Arg.Set Clflags.debug_rules, " show rules"
  ; "-ddep-path", Arg.Set Clflags.debug_dep_path, " show depency path of errors"
  ; "-dfindlib", Arg.Set Clflags.debug_findlib, " debug findlib stuff"
  ]

let parse_args argv msg l =
  let anons = ref [] in
  try
    Arg.parse_argv argv (Arg.align l) (fun x -> anons := x :: !anons) msg;
    List.rev !anons
  with
  | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 2
  | Arg.Help msg -> Printf.printf "%s" msg; exit 0

let parse_args1 argv msg l =
  match parse_args argv msg l with
  | [x] -> x
  | _ ->
    Printf.eprintf "no enough arguments\nUsage: %s\n" msg;
    exit 2

let internal argv =
  match Array.to_list argv with
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

let external_lib_deps_cmd argv =
  let packages =
    parse_args argv "jbuild external-lib-deps PACKAGES"
      common_args
  in
  let deps =
    Path.Map.fold (external_lib_deps ~packages) ~init:String_map.empty
      ~f:(fun ~key:_ ~data:deps acc -> Build.merge_lib_deps deps acc)
  in
  String_map.iter deps ~f:(fun ~key:n ~data ->
    match (data : Build.lib_dep_kind) with
    | Required -> Printf.printf "%s\n" n
    | Optional -> Printf.printf "%s (optional)\n" n)

let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  let compact () =
    Array.append
      [|sprintf "%s %s" argv.(0) argv.(1)|]
      (Array.sub argv ~pos:2 ~len:(argc - 2))
  in
  if argc >= 2 then
    match argv.(1) with
    | "internal" -> internal (compact ())
    | "build-package" ->
      let pkg =
        parse_args1 (compact ()) "jbuild build-package PACKAGE"
          common_args
      in
      Future.Scheduler.go
        (setup () >>= fun (bs, _, _) ->
         Build_system.do_build_exn bs [Path.(relative root) (pkg ^ ".install")])
    | "external-lib-deps" ->
      external_lib_deps_cmd (compact ())
    | _ ->
      let targets = parse_args argv "jbuild TARGETS" common_args in
      Future.Scheduler.go
        (setup () >>= fun (bs, _, ctx) ->
         let targets = List.map targets ~f:(Path.relative ctx.build_dir) in
         Build_system.do_build_exn bs targets)

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
