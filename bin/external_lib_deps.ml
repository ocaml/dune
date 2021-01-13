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

let all_lib_deps ~request =
  let targets = Build_system.static_deps_of_request request in
  let rules = Build_system.rules_for_transitive_closure targets in
  let lib_deps =
    List.map rules ~f:(fun (rule : Dune_engine.Rule.t) ->
        let deps = Lib_deps_info.lib_deps rule.action.build in
        (rule, deps))
  in
  let module Context_name = Dune_engine.Context_name in
  let contexts = Build_system.contexts () in
  List.fold_left lib_deps ~init:[]
    ~f:(fun acc ((rule : Dune_engine.Rule.t), deps) ->
      if Lib_name.Map.is_empty deps then
        acc
      else
        match Path.Build.extract_build_context rule.dir with
        | None -> acc
        | Some (context, p) ->
          let context = Context_name.of_string context in
          (context, (p, deps)) :: acc)
  |> Context_name.Map.of_list_multi
  |> Context_name.Map.filteri ~f:(fun ctx _ ->
         Context_name.Map.mem contexts ctx)
  |> Context_name.Map.map
       ~f:(Path.Source.Map.of_list_reduce ~f:Lib_deps_info.merge)

let opam_install_command ?switch_name packages =
  let cmd =
    match switch_name with
    | Some name -> Printf.sprintf "opam install --switch=%s" name
    | None -> "opam install"
  in
  cmd :: packages |> String.concat ~sep:" "

let run ~lib_deps ~by_dir ~setup ~only_missing ~sexp =
  Dune_engine.Context_name.Map.foldi lib_deps ~init:false
    ~f:(fun context_name lib_deps_by_dir acc ->
      let lib_deps =
        Path.Source.Map.values lib_deps_by_dir
        |> List.fold_left ~init:Lib_name.Map.empty ~f:Lib_deps_info.merge
      in
      let sctx =
        Dune_engine.Context_name.Map.find_exn setup.Import.Main.scontexts
          context_name
      in
      let switch_name =
        match (Super_context.context sctx).Context.kind with
        | Default -> None
        | Opam { switch; _ } -> Some switch
      in
      let internals = Super_context.internal_lib_names sctx in
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
              Dune_engine.Context_name.equal c.name context_name)
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
                   (Dune_engine.Context_name.to_string context_name)
               ; pp_external_libs missing
               ]);
          false
        ) else
          let required_package_names =
            Lib_name.Map.to_list missing
            |> List.filter_map ~f:(fun (name, kind) ->
                   match (kind : Lib_deps_info.Kind.t) with
                   | Optional -> None
                   | Required -> Some (Lib_name.package_name name))
            |> Package.Name.Set.of_list |> Package.Name.Set.to_list
            |> List.map ~f:Package.Name.to_string
          in
          User_message.prerr
            (User_error.make
               [ Pp.textf
                   "The following libraries are missing in the %s context:"
                   (Dune_engine.Context_name.to_string context_name)
               ; pp_external_libs missing
               ]
               ~hints:
                 [ Dune_engine.Utils.pp_command_hint
                     (opam_install_command ?switch_name required_package_names)
                 ]);
          true
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
        Format.printf "%a@." Pp.to_fmt
          (Sexp.pp
             (List
                [ Atom (Dune_engine.Context_name.to_string context_name); sexp ]));
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
                 (Dune_engine.Context_name.to_string context_name)
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
  Common.set_common common ~targets:[] ~external_lib_deps_mode:true;
  let setup, lib_deps =
    Scheduler.go ~common (fun () ->
        let open Fiber.O in
        let+ setup = Import.Main.setup common in
        let targets = Target.resolve_targets_exn common setup targets in
        let request = Target.request targets in
        let deps = all_lib_deps ~request in
        (setup, deps))
  in
  let failure = run ~by_dir ~setup ~lib_deps ~sexp ~only_missing in
  if failure then raise Dune_util.Report_error.Already_reported

let command = (term, info)
