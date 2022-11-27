open Import
open Dep_gen.Modules_data

type t = { loc : Loc.t }

let codept_syntax =
  Dune_lang.Syntax.create ~name:"codept"
    ~desc:"the codept extension (experimental)" ~experimental:true
    [ ((0, 1), `Since (3, 5)) ]
(* TODO: correct since *)

let codept_decode : t Dune_lang.Decoder.t =
  let open Dune_lang.Decoder in
  let+ loc = loc in
  { loc }

let codept_to_dyn { loc } =
  let open Dyn in
  record [ ("loc", Loc.to_dyn loc) ]

let codept_extension =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register codept_syntax
    (let+ x = codept_decode in
     (x, []))
    codept_to_dyn

let codept_prog ~project ~dir sctx =
  let { loc } =
    Option.value_exn (Dune_project.find_extension_args project codept_extension)
  in
  Super_context.resolve_program sctx ~dir ~loc:(Some loc) "codept"
    ~hint:"opam install codept"

let codept_o_arg name target : _ Command.Args.t list =
  [ A "-o"; Target target; A name ]

let deps_of
    ({ sandbox; modules; sctx; dir; obj_dir; vimpl = _; stdlib = _; project } as
    md) ~ml_kind unit =
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
  let* codept = codept_prog ~project ~dir sctx in
  let* () =
    (* 1. Generate m2l and approx immediate from source. *)
    Super_context.add_rule sctx ~dir
      (let open Action_builder.With_targets.O in
      Command.run codept
        ~dir:(Path.build context.build_dir)
        [ As [ "-k"; "-verbosity"; "error" ]
        ; Command.Args.dyn flags
        ; Command.Ml_kind.flag ml_kind
        ; Dep (Module.File.path source)
        ; S (codept_o_arg "-m2l" m2l_file)
        ; S (codept_o_arg "-nl-modules" approx_dep_file)
        ]
      >>| Action.Full.add_sandbox sandbox)
  in
  let* () =
    (* 2. Generate immediate and sig from m2l and approx immediate sigs. *)
    let { Modules.vlib = vlib_modules; _ } = Modules.split_by_lib modules in
    let is_vlib_module m =
      (* TODO: better way to check this? or could include vlib modules? *)
      List.exists vlib_modules ~f:(fun vm ->
          Module_name.Unique.compare (Module.obj_name vm) (Module.obj_name m)
          = Eq)
    in
    let build_paths dependencies =
      let dependency_file_path m =
        if Module.kind m = Alias || is_vlib_module m then None
        else Some (Path.build (sig_file m))
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
      let sig_args : _ Command.Args.t list =
        if gen_sig then codept_o_arg "-sig" (sig_file unit) else []
      in
      let open Action_builder.With_targets.O in
      Command.run codept
        ~dir:(Path.build context.build_dir)
        [ As [ "-k"; "-verbosity"; "error" ]
          (* avoid self-cycle errors and unresolved module notifications *)
        ; Command.Args.dyn flags
        ; Concat (":", [ Dep (Path.build m2l_file); A (Module_name.to_string (Module.name unit)) ])
        (* ; Dep (Path.build m2l_file) *)
        ; Dyn path_args
        ; S sig_args
        ; S (codept_o_arg "-modules" immediate_file)
        ]
      >>| Action.Full.add_sandbox sandbox
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
