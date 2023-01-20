open Import
open Dep_gen.Modules_data

let deps_of
    ({ sandbox
     ; modules
     ; sctx
     ; dir
     ; obj_dir
     ; vimpl = _
     ; stdlib = _
     ; project = _
     } as md) ~ml_kind unit =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir in
  let context = Super_context.context sctx in
  let ocamldep_output = dep (Immediate (unit, ml_kind)) in
  let open Memo.O in
  let* () =
    (* 1. Generate immediate from source. *)
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
  in
  let+ () =
    (* 2. Merge transitives. *)
    let file = Module.File.path source in
    Dep_gen.transitive_of_immediate_rule md ~ml_kind ~file unit
  in
  Dep_gen.read_deps_of ~obj_dir ~modules ~ml_kind unit

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let file = Module.File.path source in
    Dep_gen.read_immediate_deps_of_source ~obj_dir ~modules ~file ~ml_kind unit
