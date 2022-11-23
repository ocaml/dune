open Import
open Dep_gen.Modules_data

let codept_prog ~dir sctx =
  Super_context.resolve_program sctx ~dir ~loc:None "codept"
    ~hint:"opam install codept"

let deps_of
    ({ sandbox; modules; sctx; dir; obj_dir; vimpl = _; stdlib = _ } as md)
    ~ml_kind unit =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir in
  let context = Super_context.context sctx in
  let parse_module_names = Dep_gen.parse_module_names ~modules in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  let ocamldep_output = dep (Immediate source) in
  let m2l_file = dep (M2l (unit, ml_kind)) in
  let approx_dep_file = dep (Immediate_approx source) in
  let sig_file m = dep (Sig m) in
  let gen_sig =
    match ml_kind with
    | Intf -> true
    | Impl -> not (Module.has unit ~ml_kind:Intf)
  in
  let open Memo.O in
  let* codept = codept_prog ~dir sctx in
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
    let {Modules.vlib = vlib_modules; _} = Modules.split_by_lib modules in
    let build_paths dependencies =
      List.filter_map dependencies ~f:(fun dependency ->
          (* Format.printf "%a\n" Pp.to_fmt (Dyn.pp (Module.to_dyn dependency)); *)
          if Module.kind dependency = Alias || List.exists ~f:(fun m -> Module_name.Unique.compare (Module.obj_name m) (Module.obj_name dependency) = Eq) vlib_modules then
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
          |> Dep_gen.interpret_deps md ~unit
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
      Command.run codept
        ~dir:(Path.build context.build_dir)
        [ As ["-k"; "-verbosity"; "error"] (* avoid self-cycle errors and unresolved module notifications *)
        ; Command.Args.dyn flags
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
      >>| Action.Full.add_sandbox sandbox)
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
        Dep_gen.parse_deps_exn ~file:(Path.build m2l_file) lines
        |> Dep_gen.interpret_deps md ~unit
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
       ~f:(Dep_gen.parse_module_names ~unit ~modules)
       (Action_builder.lines_of (Path.build all_deps_file)))

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let ocamldep_output = Obj_dir.Module.dep obj_dir (Immediate source) in
    let m2l_file = Obj_dir.Module.dep obj_dir (M2l (unit, ml_kind)) in
    Action_builder.memoize
      (Path.Build.to_string ocamldep_output)
      (Action_builder.map
         ~f:(fun lines ->
           Dep_gen.parse_deps_exn ~file:(Path.build m2l_file) lines
           |> Dep_gen.parse_module_names ~unit ~modules)
         (Action_builder.lines_of (Path.build ocamldep_output)))
