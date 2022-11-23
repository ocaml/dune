open Import

open Ocamldep.Modules_data

let parse_module_names ~(unit : Module.t) ~modules words =
  List.filter_map words ~f:(fun m ->
      let m = Module_name.of_string m in
      Modules.find_dep modules ~of_:unit m)

let parse_deps_exn ~file lines =
  let invalid () =
    User_error.raise
      [ Pp.textf "ocamldep returned unexpected output for %s:"
          (Path.to_string_maybe_quoted file)
      ; Pp.vbox
          (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
               Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
      ]
  in
  match lines with
  | [] | _ :: _ :: _ -> invalid ()
  | [ line ] -> (
    match String.lsplit2 line ~on:':' with
    | None -> invalid ()
    | Some (basename, deps) ->
      let basename = Filename.basename basename in
      if basename <> Path.basename file then invalid ();
      String.extract_blank_separated_words deps)

let interpret_deps md ~unit deps =
  let dir = md.dir in
  let modules = md.modules in
  let deps = parse_module_names ~unit ~modules deps in
  if Option.is_none md.stdlib then
    Modules.main_module_name modules
    |> Option.iter ~f:(fun (main_module_name : Module_name.t) ->
           if
             Module_name.Infix.(Module.name unit <> main_module_name)
             && (not (Module.kind unit = Alias))
             && List.exists deps ~f:(fun x -> Module.name x = main_module_name)
           then
             User_error.raise
               [ Pp.textf "Module %s in directory %s depends on %s."
                   (Module_name.to_string (Module.name unit))
                   (Path.to_string_maybe_quoted (Path.build dir))
                   (Module_name.to_string main_module_name)
               ; Pp.textf "This doesn't make sense to me."
               ; Pp.nop
               ; Pp.textf
                   "%s is the main module of the library and is the only \
                    module exposed outside of the library. Consequently, it \
                    should be the one depending on all the other modules in \
                    the library."
                   (Module_name.to_string main_module_name)
               ]);
  match Modules.alias_for modules unit with
  | None -> deps
  | Some m -> m :: deps

let deps_of
    ({ sandbox; modules; sctx; dir; obj_dir; vimpl = _; stdlib = _ } as md)
    ~ml_kind unit =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir in
  let context = Super_context.context sctx in
  let parse_module_names = parse_module_names ~modules in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  let ocamldep_output = dep (Immediate source) in
  (* let m2l_file = Path.Build.relative (Obj_dir.obj_dir obj_dir) (Path.basename (Module.File.path source) ^ ".m2l") in *)
  let m2l_file = Path.Build.relative (Obj_dir.obj_dir obj_dir) (Module_name.to_string (Module.name unit) ^ Ml_kind.choose ~impl:".m2l" ~intf:".m2li" ml_kind) in
  let approx_dep_file = Path.Build.relative (Obj_dir.obj_dir obj_dir) (Path.basename (Module.File.path source) ^ ".approx-d") in
  let sig_file m =
    Path.Build.relative (Obj_dir.obj_dir obj_dir) (Module_name.to_string (Module.name m) ^ ".sig")
  in
  let gen_sig =
    match ml_kind with
    | Intf -> true
    | Impl -> not (Module.has unit ~ml_kind:Intf)
  in
  let open Memo.O in
  let* codept = Context.which context "codept" in
  let codept' = Option.value_exn codept in
  let codept = Ok (Option.value_exn codept) in
  let* () =
    Super_context.add_rule sctx ~dir
      (let open Action_builder.With_targets.O in
      let flags, sandbox =
        Option.value (Module.pp_flags unit)
          ~default:(Action_builder.return [], sandbox)
      in
      Command.run codept
        ~dir:(Path.build context.build_dir)
        [ As ["-k"; "-verbosity"; "error"]
        ; Command.Args.dyn flags
        ; Command.Ml_kind.flag ml_kind
        ; Dep (Module.File.path source)
        ; A "-o"
        ; Target m2l_file
        ; A "-m2l"
        ; A "-o"
        ; Target approx_dep_file
        ; A "-nl-modules"
        ]
      >>| Action.Full.add_sandbox sandbox)
  in
  let* () =
    let build_paths dependencies =
      List.filter_map dependencies ~f:(fun dependency ->
          if Module.kind dependency = Alias then
            None
          else
            Some (sig_file dependency)
        )
    in
    let action =
      let paths =
        let open Action_builder.O in
        let+ lines = Action_builder.lines_of (Path.build approx_dep_file) in
        let modules =
          lines
          |> interpret_deps md ~unit
        in
        build_paths modules
        |> List.map ~f:Path.build
        |> (fun x -> Command.Args.Deps x)
      in
      (* let paths = List.map ~f:Path.build paths in *)
      (* Action_builder.with_file_targets ~file_targets:[ all_deps_file ]
        (let+ sources, extras =
          Action_builder.dyn_paths
            (let+ sources, extras = paths in
              ((sources, extras), sources))
        in
        Action.Merge_files_into (sources, extras, all_deps_file)) *)
      (let open Action_builder.With_targets.O in
      let flags, sandbox =
        Option.value (Module.pp_flags unit)
          ~default:(Action_builder.return [], sandbox)
      in
      (* let+ paths = Action_builder.with_no_targets paths in *)
      let dir = Path.build context.build_dir in
      let args =
        [ Command.Args.dyn flags
        (* ; Command.Ml_kind.flag ml_kind *)
        ; Dep (Path.build m2l_file)
        ; Dyn paths
        ; S (if gen_sig then
          [ A "-o"
          ; Target (sig_file unit)
          ; A "-sig"
          ]
          else [])
        ; A "-o"
        ; Target ocamldep_output
        ; A "-modules"
        ]
      in
      (* Command.run codept
        ~dir:(Path.build context.build_dir)
        [ Command.Args.dyn flags
        ; Command.Ml_kind.flag ml_kind
        ; Dep (Path.build m2l_file)
        ; Deps paths
        ; A "-o"
        ; Target (sig_file unit)
        ; A "-sig"
        ; A "-o"
        ; Target ocamldep_output
        ; A "-modules"
        ]
      >>| Action.Full.add_sandbox sandbox) *)
      let+ () = Action_builder.with_no_targets (Action_builder.path codept')
      and+ args = Command.expand ~dir (S args) in
      let action = Action.run codept args in
      Action.Full.make ~sandbox (Action.chdir dir action))
    in
    Super_context.add_rule sctx ~dir action
  in
  (* let* () =
    Super_context.add_rule sctx ~dir
      (let open Action_builder.With_targets.O in
      let flags, sandbox =
        Option.value (Module.pp_flags unit)
          ~default:(Action_builder.return [], sandbox)
      in
      Command.run context.ocamldep
        ~dir:(Path.build context.build_dir)
        ~stdout_to:ocamldep_output
        [ A "-modules"
        ; Command.Args.dyn flags
        ; Command.Ml_kind.flag ml_kind
        ; Dep (Module.File.path source)
        ]
      >>| Action.Full.add_sandbox sandbox)
  in *)
  let build_paths dependencies =
    let dependency_file_path m =
      let ml_kind m =
        if Module.kind m = Alias then None
        else if Module.has m ~ml_kind:Intf then Some Ml_kind.Intf
        else Some Impl
      in
      ml_kind m
      |> Option.map ~f:(fun ml_kind ->
             Path.build (dep (Transitive (m, ml_kind))))
    in
    List.filter_map dependencies ~f:dependency_file_path
  in
  let action =
    let open Action_builder.O in
    let paths =
      let+ lines = Action_builder.lines_of (Path.build ocamldep_output) in
      let modules =
        (* parse_deps_exn ~file:(Module.File.path source) lines *)
        parse_deps_exn ~file:(Path.build m2l_file) lines
        |> interpret_deps md ~unit
      in
      ( build_paths modules
      , List.map modules ~f:(fun m -> Module_name.to_string (Module.name m)) )
    in
    Action_builder.with_file_targets ~file_targets:[ all_deps_file ]
      (let+ sources, extras =
         Action_builder.dyn_paths
           (let+ sources, extras = paths in
            ((sources, extras), sources))
       in
       Action.Merge_files_into (sources, extras, all_deps_file))
  in
  let+ () =
    Super_context.add_rule sctx ~dir
      (Action_builder.With_targets.map ~f:Action.Full.make action)
  in
  let all_deps_file = Path.build all_deps_file in
  Action_builder.memoize
    (Path.to_string all_deps_file)
    (Action_builder.map ~f:(parse_module_names ~unit)
       (Action_builder.lines_of all_deps_file))

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let all_deps_file = Obj_dir.Module.dep obj_dir (Transitive (unit, ml_kind)) in
  Action_builder.memoize
    (Path.Build.to_string all_deps_file)
    (Action_builder.map
       ~f:(parse_module_names ~unit ~modules)
       (Action_builder.lines_of (Path.build all_deps_file)))

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let ocamldep_output = Obj_dir.Module.dep obj_dir (Immediate source) in
    Action_builder.memoize
      (Path.Build.to_string ocamldep_output)
      (Action_builder.map
         ~f:(fun lines ->
           parse_deps_exn ~file:(Module.File.path source) lines
           |> parse_module_names ~unit ~modules)
         (Action_builder.lines_of (Path.build ocamldep_output)))
