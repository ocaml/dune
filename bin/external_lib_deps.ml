open Stdune
open Import

let format_external_libs libs =
  Lib_name.Map.to_list libs
  |> List.map ~f:(fun (name, kind) ->
    match (kind : Lib_deps_info.Kind.t) with
    | Optional -> sprintf "- %s (optional)" (Lib_name.to_string name)
    | Required -> sprintf "- %s" (Lib_name.to_string name))
  |> String.concat ~sep:"\n"

let doc = "Print out external libraries needed to build the given targets."

let man =
  [ `S "DESCRIPTION"
  ; `P {|Print out the external libraries needed to build the given targets.|}
  ; `P {|The output of $(b,jbuild external-lib-deps @install) should be included
          in what is written in your $(i,<package>.opam) file.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "external-lib-deps" ~doc ~man

let run ~lib_deps ~by_dir ~setup ~only_missing ~sexp =
  String.Map.foldi lib_deps ~init:false
    ~f:(fun context_name lib_deps_by_dir acc ->
      let lib_deps =
        Path.Source.Map.values lib_deps_by_dir
        |> List.fold_left ~init:Lib_name.Map.empty ~f:Lib_deps_info.merge
      in
      let internals =
        String.Map.find_exn setup.Import.Main.scontexts context_name
        |> Super_context.internal_lib_names
      in
      let is_external name _kind = not (Lib_name.Set.mem internals name) in
      let externals =
        Lib_name.Map.filteri lib_deps ~f:is_external
      in
      if only_missing then begin
        if by_dir || sexp then
          die "@{<error>Error@}: --only-missing cannot be used with \
               --unstable-by-dir or --sexp";
        let context =
          List.find_exn setup.workspace.contexts
            ~f:(fun c -> c.name = context_name)
        in
        let missing =
          Lib_name.Map.filteri externals ~f:(fun name _ ->
            not (Findlib.available context.findlib name))
        in
        if Lib_name.Map.is_empty missing then
          acc
        else if Lib_name.Map.for_alli missing
                  ~f:(fun _ kind -> kind = Lib_deps_info.Kind.Optional)
        then begin
          Format.eprintf
            "@{<error>Error@}: The following libraries are missing \
             in the %s context:\n\
             %s@."
            context_name
            (format_external_libs missing);
          false
        end else begin
          Format.eprintf
            "@{<error>Error@}: The following libraries are missing \
             in the %s context:\n\
             %s\n\
             Hint: try: opam install %s@."
            context_name
            (format_external_libs missing)
            (Lib_name.Map.to_list missing
             |> List.filter_map ~f:(fun (name, kind) ->
               match (kind : Lib_deps_info.Kind.t) with
               | Optional -> None
               | Required -> Some (Lib_name.package_name name))
             |> Package.Name.Set.of_list
             |> Package.Name.Set.to_list
             |> List.map ~f:Package.Name.to_string
             |> String.concat ~sep:" ");
          true
        end
      end else if sexp then begin
        if not by_dir then
          die "@{<error>Error@}: --sexp requires --unstable-by-dir";
        let lib_deps_by_dir =
          lib_deps_by_dir
          |> Path.Source.Map.map ~f:(Lib_name.Map.filteri ~f:is_external)
          |> Path.Source.Map.filter ~f:(fun m -> not (Lib_name.Map.is_empty m))
        in
        let sexp =
          Path.Source.Map.to_dyn Lib_deps_info.to_dyn lib_deps_by_dir
          |> Dyn.to_sexp
        in
        Format.printf "%a@." Sexp.pp (List [Atom context_name; sexp]);
        acc
      end else begin
        if by_dir then
          die "@{<error>Error@}: --unstable-by-dir cannot be used without --sexp";
        Printf.printf
          "These are the external library dependencies in the %s context:\n\
           %s\n%!"
          context_name
          (format_external_libs externals);
        acc
      end)

let term =
  let+ common = Common.term
  and+ only_missing =
    Arg.(value
         & flag
         & info ["missing"]
             ~doc:{|Only print out missing dependencies|})
  and+ targets =
    Arg.(non_empty
         & pos_all string []
         & Arg.info [] ~docv:"TARGET")
  and+ by_dir =
    Arg.(value
         & flag
         & info ["unstable-by-dir"]
             ~doc:{|Print dependencies per directory
                    (this feature is currently unstable)|})
  and+ sexp =
    Arg.(value
         & flag
         & info ["sexp"]
             ~doc:{|Produce a s-expression output|})
  in
  Common.set_common common ~targets:[];
  let log = Log.create common in
  let setup, lib_deps =
    Scheduler.go ~log ~common (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup ~log common ~external_lib_deps_mode:true in
      let targets = Target.resolve_targets_exn ~log common setup targets in
      let request = Target.request setup targets in
      let+ deps = Build_system.all_lib_deps ~request in
      (setup, deps))
  in
  let failure = run ~by_dir ~setup ~lib_deps ~sexp ~only_missing in
  if failure then raise Dune.Import.Already_reported

let command = term, info
