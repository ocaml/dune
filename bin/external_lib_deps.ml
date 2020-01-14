open Stdune
open Import

let pp_external_libs libs =
  Pp.enumerate (Lib_name.Map.to_list libs) ~f:(fun (name, kind) ->
      match (kind : Lib_deps_info.Kind.t) with
      | Optional -> Pp.textf "%s (optional)" (Lib_name.to_string name)
      | Required -> Pp.textf "%s" (Lib_name.to_string name))

let doc = "Print out external libraries needed to build the given targets."

let man =
  [ `S "DESCRIPTION"
  ; `P {|Print out the external libraries needed to build the given targets.|}
  ; `P
      {|The output of $(b,dune external-lib-deps @install) should be included
          in what is written in your $(i,<package>.opam) file.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "external-lib-deps" ~doc ~man

let run ~lib_deps ~by_dir ~setup ~only_missing ~sexp =
  Dune.Context_name.Map.foldi lib_deps ~init:false
    ~f:(fun context_name lib_deps_by_dir acc ->
      let lib_deps =
        Path.Source.Map.values lib_deps_by_dir
        |> List.fold_left ~init:Lib_name.Map.empty ~f:Lib_deps_info.merge
      in
      let internals =
        Dune.Context_name.Map.find_exn setup.Import.Main.scontexts context_name
        |> Super_context.internal_lib_names
      in
      let is_external name _kind = not (Lib_name.Set.mem internals name) in
      let externals = Lib_name.Map.filteri lib_deps ~f:is_external in
      if only_missing then (
        if by_dir || sexp then
          User_error.raise
            [ Pp.textf
                "--only-missing cannot be used with --unstable-by-dir or --sexp"
            ];
        let context =
          List.find_exn setup.workspace.contexts ~f:(fun c ->
              Dune.Context_name.equal c.name context_name)
        in
        let missing =
          Lib_name.Map.filteri externals ~f:(fun name _ ->
              not (Findlib.available context.findlib name))
        in
        if Lib_name.Map.is_empty missing then
          acc
        else if
          Lib_name.Map.for_alli missing ~f:(fun _ kind ->
              kind = Lib_deps_info.Kind.Optional)
        then (
          User_message.prerr
            (User_error.make
               [ Pp.textf
                   "The following libraries are missing in the %s context:"
                   (Dune.Context_name.to_string context_name)
               ; pp_external_libs missing
               ]);
          false
        ) else (
          User_message.prerr
            (User_error.make
               [ Pp.textf
                   "The following libraries are missing in the %s context:"
                   (Dune.Context_name.to_string context_name)
               ; pp_external_libs missing
               ]
               ~hints:
                 [ Pp.concat ~sep:Pp.space
                     ( Pp.textf "try: opam install"
                     :: ( Lib_name.Map.to_list missing
                        |> List.filter_map ~f:(fun (name, kind) ->
                               match (kind : Lib_deps_info.Kind.t) with
                               | Optional -> None
                               | Required -> Some (Lib_name.package_name name))
                        |> Package.Name.Set.of_list |> Package.Name.Set.to_list
                        |> List.map ~f:(fun p ->
                               Pp.verbatim (Package.Name.to_string p)) ) )
                 ]);
          true
        )
      ) else if sexp then (
        if not by_dir then
          User_error.raise [ Pp.textf "--sexp requires --unstable-by-dir" ];
        let lib_deps_by_dir =
          lib_deps_by_dir
          |> Path.Source.Map.map ~f:(Lib_name.Map.filteri ~f:is_external)
          |> Path.Source.Map.filter ~f:(fun m -> not (Lib_name.Map.is_empty m))
        in
        let sexp =
          Path.Source.Map.to_dyn Lib_deps_info.to_dyn lib_deps_by_dir
          |> Sexp.of_dyn
        in
        Format.printf "%a@." Sexp.pp
          (List [ Atom (Dune.Context_name.to_string context_name); sexp ]);
        acc
      ) else (
        if by_dir then
          User_error.raise
            [ Pp.textf "--unstable-by-dir cannot be used without --sexp" ];
        User_message.print
          (User_message.make
             [ Pp.textf
                 "These are the external library dependencies in the %s \
                  context:"
                 (Dune.Context_name.to_string context_name)
             ; pp_external_libs externals
             ]);
        acc
      ))

let term =
  let+ common = Common.term
  and+ only_missing =
    Arg.(
      value & flag
      & info [ "missing" ] ~doc:{|Only print out missing dependencies|})
  and+ targets = Arg.(non_empty & pos_all dep [] & Arg.info [] ~docv:"TARGET")
  and+ by_dir =
    Arg.(
      value & flag
      & info [ "unstable-by-dir" ]
          ~doc:
            {|Print dependencies per directory
                    (this feature is currently unstable)|})
  and+ sexp =
    Arg.(value & flag & info [ "sexp" ] ~doc:{|Produce a s-expression output|})
  in
  Common.set_common common ~targets:[];
  let setup, lib_deps =
    Scheduler.go ~common (fun () ->
        let open Fiber.O in
        let+ setup = Import.Main.setup common ~external_lib_deps_mode:true in
        let targets = Target.resolve_targets_exn common setup targets in
        let request = Target.request targets in
        let deps = Build_system.all_lib_deps ~request in
        (setup, deps))
  in
  let failure = run ~by_dir ~setup ~lib_deps ~sexp ~only_missing in
  if failure then raise Dune.Report_error.Already_reported

let command = (term, info)
