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
  let immediate_file = dep (Immediate source) in
  let m2l_file = dep (M2l (unit, ml_kind)) in
  let approx_dep_file = dep (Immediate_approx source) in
  let sig_file m = dep (Sig m) in
  let gen_sig =
    match ml_kind with
    | Intf -> true
    | Impl -> not (Module.has unit ~ml_kind:Intf)
  in
  let flags, sandbox =
    Option.value (Module.pp_flags unit)
      ~default:(Action_builder.return [], sandbox)
  in
  let open Memo.O in
  let* codept = codept_prog ~dir sctx in
  let* () =
    (* 1. Generate m2l and approx immediate from source. *)
    Super_context.add_rule sctx ~dir
      (let open Action_builder.With_targets.O in
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
    (* 2. Generate immediate and sig from m2l and approx immediate sigs. *)
    let {Modules.vlib = vlib_modules; _} = Modules.split_by_lib modules in
    let is_vlib_module m = (* TODO: better way to check this? or could include vlib modules? *)
      List.exists vlib_modules ~f:(fun vm ->
          Module_name.Unique.compare (Module.obj_name vm) (Module.obj_name m) = Eq
        )
    in
    let build_paths dependencies =
      let dependency_file_path m =
        if Module.kind m = Alias || is_vlib_module m then
          None
        else
          Some (Path.build (sig_file m))
      in
      List.filter_map dependencies ~f:dependency_file_path
    in
    let action =
      let open Action_builder.O in
      let paths =
        let+ lines = Action_builder.lines_of (Path.build approx_dep_file) in
        let modules = Dep_gen.interpret_deps md ~unit lines in
        build_paths modules
      in
      let path_args =
        let+ paths = paths in
        Command.Args.Deps paths
      in
      let sig_args: _ Command.Args.t list =
        if gen_sig then
          [ A "-o"
          ; Target (sig_file unit)
          ; A "-sig"
          ]
        else
          []
      in
      (let open Action_builder.With_targets.O in
      Command.run codept
        ~dir:(Path.build context.build_dir)
        [ As ["-k"; "-verbosity"; "error"] (* avoid self-cycle errors and unresolved module notifications *)
        ; Command.Args.dyn flags
        ; Dep (Path.build m2l_file)
        ; Dyn path_args
        ; S sig_args
        ; A "-o"
        ; Target immediate_file
        ; A "-modules"
        ]
      >>| Action.Full.add_sandbox sandbox)
    in
    Super_context.add_rule sctx ~dir action
  in
  let+ () =
    (* 3. Merge transitives. *)
    let file = Path.build m2l_file in
    Dep_gen.transitive_of_immediate_rule md ~ml_kind ~source ~file unit
  in
  Dep_gen.read_deps_of ~obj_dir ~modules ~ml_kind unit

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let dep = Obj_dir.Module.dep obj_dir in
    let file = Path.build (dep (M2l (unit, ml_kind))) in
    Dep_gen.read_immediate_deps_of_source ~obj_dir ~modules ~source ~file unit
