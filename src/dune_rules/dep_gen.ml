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
    ; project : Dune_project.t
    }
end

open Modules_data

let parse_module_names ~(unit : Module.t) ~modules words =
  (* TODO: move this codept thing out *)
  let um = Modules.fold_user_available modules ~f:(fun m acc ->
      match Modules.find_dep modules ~of_:unit (Module.name m) with (* FIXME: stupid way to find all deps *)
      | Some _ ->
        Module_name.Unique.Map.add_exn acc (Module.obj_name m) m
      | None -> acc
    ) ~init:Module_name.Unique.Map.empty
  in
  List.filter_map words ~f:(fun m ->
      let s = m in
      let m = Module_name.of_string m in
      match Modules.find_dep modules ~of_:unit m with
      | Some d -> Some d
      | None ->
        let u = Module_name.Unique.of_string s in
        Module_name.Unique.Map.find um u)

let parse_deps_exn ~file lines =
  let invalid () =
    User_error.raise
      [ Pp.textf
          "ocamldep returned unexpected output for %s:" (* TODO: generalize *)
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

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let dep = Obj_dir.Module.dep obj_dir in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  Action_builder.memoize
    (Path.Build.to_string all_deps_file)
    (Action_builder.map
       ~f:(parse_module_names ~unit ~modules)
       (Action_builder.lines_of (Path.build all_deps_file)))

let read_immediate_deps_of_source ~obj_dir ~modules ~source ~file unit =
  let dep = Obj_dir.Module.dep obj_dir in
  let immediate_file = dep (Immediate source) in
  Action_builder.memoize
    (Path.Build.to_string immediate_file)
    (Action_builder.map
       ~f:(fun lines ->
         parse_deps_exn ~file lines |> parse_module_names ~unit ~modules)
       (Action_builder.lines_of (Path.build immediate_file)))

let transitive_of_immediate_rule
    ({ sctx
     ; dir
     ; obj_dir
     ; sandbox = _
     ; modules = _
     ; vimpl = _
     ; stdlib = _
     ; project = _
     } as md) ~ml_kind ~source ~file unit =
  let dep = Obj_dir.Module.dep obj_dir in
  let immediate_file = dep (Immediate source) in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
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
      let+ lines = Action_builder.lines_of (Path.build immediate_file) in
      let modules = parse_deps_exn ~file lines |> interpret_deps md ~unit in
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
  Super_context.add_rule sctx ~dir
    (Action_builder.With_targets.map ~f:Action.Full.make action)

module type S = sig
  val deps_of :
       Modules_data.t
    -> ml_kind:Ml_kind.t
    -> Module.t
    -> Module.t list Action_builder.t Memo.t

  val read_immediate_deps_of :
       obj_dir:Path.Build.t Obj_dir.t
    -> modules:Modules.t
    -> ml_kind:Ml_kind.t
    -> Module.t
    -> Module.t list Action_builder.t
end
