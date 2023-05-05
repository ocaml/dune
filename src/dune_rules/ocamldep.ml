open Import

module Modules_data = struct
  type t =
    { dir : Path.Build.t
    ; obj_dir : Path.Build.t Obj_dir.t
    ; sctx : Super_context.t
    ; vimpl : Vimpl.t option
    ; modules : Modules.t
    ; stdlib : Ocaml_stdlib.t option
    ; sandbox : Sandbox_config.t
    }
end

open Modules_data

let parse_module_names ~dir ~(unit : Module.t) ~modules words =
  List.concat_map words ~f:(fun mns ->
      let mname = Module_name.of_string mns in
      match Modules.find_dep modules ~of_:unit mname with
      | Ok [] ->
        [ Module_dep.External (Module_dep.External_name.of_string mns) ]
      | Ok s -> List.map s ~f:(fun x -> Module_dep.Local x)
      | Error `Parent_cycle ->
        User_error.raise
          [ Pp.textf "Module %s in directory %s depends on %s."
              (Module_name.to_string (Module.name unit))
              (Path.to_string_maybe_quoted (Path.build dir))
              (Module_name.to_string mname)
          ; Pp.textf "This doesn't make sense to me."
          ; Pp.nop
          ; Pp.textf
              "%s is the main module of the library and is the only module \
               exposed outside of the library. Consequently, it should be the \
               one depending on all the other modules in the library."
              (Module_name.to_string mname)
          ])

let parse_compilation_units ~modules x y =
  let loc =
    let obj_map = Modules.obj_map modules in
    List.filter_map x ~f:(fun m ->
        let obj_name = Module_name.Unique.of_string m in
        match Module_name.Unique.Map.find obj_map obj_name with
        | Some m -> Some (Module_dep.Local (Modules.Sourced_module.to_module m))
        | None ->
          Some (Module_dep.External (Module_dep.External_name.of_string m)))
  in
  List.append loc
    (List.map y ~f:(fun s ->
         Module_dep.External (Module_dep.External_name.of_string s)))

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

let deps_of ({ sandbox; modules; sctx; dir; obj_dir; vimpl; stdlib = _ } as md)
    ~ml_kind unit implicit_transitive_deps =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir in
  let context = Super_context.context sctx in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  let ext_deps_file = dep (Ext (unit, ml_kind)) in
  let ocamldep_output = dep (Immediate (unit, ml_kind)) in
  let open Memo.O in
  let transitive_deps modules d =
    let transive_dep m =
      let ml_kind m =
        match Module.kind m with
        | Alias _ -> None
        | _ ->
          if Module.has m ~ml_kind:Intf then Some Ml_kind.Intf else Some Impl
      in
      ml_kind m
      |> Option.map ~f:(fun ml_kind -> Path.build (dep (d (m, ml_kind))))
    in
    List.filter_map modules ~f:transive_dep
  in
  let* () =
    Super_context.add_rule sctx ~dir
      (let open Action_builder.With_targets.O in
      let flags, sandbox =
        Option.value (Module.pp_flags unit)
          ~default:(Action_builder.return [], sandbox)
      in
      Command.run context.ocaml.ocamldep
        ~dir:(Path.build context.build_dir)
        ~stdout_to:ocamldep_output
        [ A "-modules"
        ; Command.Args.dyn flags
        ; Command.Ml_kind.flag ml_kind
        ; Dep (Module.File.path source)
        ]
      >>| Action.Full.add_sandbox sandbox)
  in
  let lines = Action_builder.lines_of (Path.build ocamldep_output) in
  let make_paths f filter =
    let open Action_builder.O in
    let+ lines = lines in
    let immediate_deps =
      let parsed = parse_deps_exn ~file:(Module.File.path source) lines in
      parsed |> parse_module_names ~dir:md.dir ~unit ~modules
    in
    let local =
      List.filter_map immediate_deps ~f:(fun m ->
          (* Maybe have kind in external dep *)
          match m with
          | Module_dep.Local m -> Some m
          | _ -> None)
    in
    let mods_name_uniq = List.filter_map immediate_deps ~f:filter in
    let ext = transitive_deps local f in
    (ext, mods_name_uniq)
  in
  let impl_vmodule =
    (match Module.kind unit with
    | Impl_vmodule -> true
    | _ -> false)
    || Option.is_some vimpl
  in

  let make_with_file_targets paths file =
    let open Action_builder.O in
    Action_builder.with_file_targets ~file_targets:[ file ]
      (let+ sources, extras =
         Action_builder.dyn_paths
           (let+ sources, extras = paths in
            ((sources, extras), sources))
       in
       Action.Merge_files_into (sources, extras, file))
  in
  let add_rule produce_all_deps =
    let rule =
      Action_builder.With_targets.map ~f:Action.Full.make produce_all_deps
    in
    let+ () = Super_context.add_rule sctx ~dir rule in
    produce_all_deps
  in
  let* _ =
    let produce_all_deps =
      let paths =
        make_paths
          (fun (a, b) -> Transitive (a, b))
          (fun m ->
            match m with
            | Module_dep.Local m ->
              Some (Module.obj_name m |> Module_name.Unique.to_string)
            | External _ -> None)
      in
      make_with_file_targets paths all_deps_file
    in
    add_rule produce_all_deps
  in
  let+ _ =
    if implicit_transitive_deps && not impl_vmodule then
      let produce_all_deps_ext =
        let paths =
          make_paths
            (fun (a, b) -> Ext (a, b))
            (fun m ->
              match m with
              | Module_dep.Local _ -> None
              | External s -> Some (Module_dep.External_name.to_string s))
        in
        make_with_file_targets paths ext_deps_file
      in
      add_rule produce_all_deps_ext
    else
      Memo.return
        (Action_builder.with_no_targets (Action_builder.return Action.empty))
  in
  let all_deps_file = Path.build all_deps_file in
  let ext_deps_file = Path.build ext_deps_file in
  let md_l =
    if implicit_transitive_deps && not impl_vmodule then
      Action_builder.map2
        ~f:(fun x y -> parse_compilation_units ~modules x y)
        (Action_builder.lines_of all_deps_file)
        (Action_builder.lines_of ext_deps_file)
    else
      Action_builder.map
        ~f:(fun x -> parse_compilation_units ~modules x [])
        (Action_builder.lines_of all_deps_file)
  in
  (Action_builder.memoize (Path.to_string all_deps_file)) md_l

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let all_deps_file = Obj_dir.Module.dep obj_dir (Transitive (unit, ml_kind)) in

  Action_builder.map
    (Action_builder.lines_of (Path.build all_deps_file))
    ~f:(fun x -> parse_compilation_units ~modules x [])
  |> Action_builder.memoize (Path.Build.to_string all_deps_file)

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let ocamldep_output =
      Obj_dir.Module.dep obj_dir (Immediate (unit, ml_kind))
    in
    Action_builder.lines_of (Path.build ocamldep_output)
    |> Action_builder.map ~f:(fun lines ->
           parse_deps_exn ~file:(Module.File.path source) lines
           |> parse_module_names ~dir:(Obj_dir.dir obj_dir) ~unit ~modules)
    |> Action_builder.memoize (Path.Build.to_string ocamldep_output)
