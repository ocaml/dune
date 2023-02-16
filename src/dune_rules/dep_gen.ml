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

let parse_module_names ~dir ~(unit : Module.t) ~modules words =
  List.concat_map words ~f:(fun m ->
      match Module_name.of_string_opt m with
      (* TODO: codept -nl-modules may output "Stdlib.Format" *)
      | Some m -> (
        match Modules.find_dep modules ~of_:unit m with
        | Ok s -> s
        | Error `Parent_cycle ->
          User_error.raise
            [ Pp.textf "Module %s in directory %s depends on %s."
                (Module_name.to_string (Module.name unit))
                (Path.to_string_maybe_quoted (Path.build dir))
                (Module_name.to_string m)
            ; Pp.textf "This doesn't make sense to me."
            ; Pp.nop
            ; Pp.textf
                "%s is the main module of the library and is the only module \
                 exposed outside of the library. Consequently, it should be \
                 the one depending on all the other modules in the library."
                (Module_name.to_string m)
            ])
      | None -> [])

let parse_compilation_units ~modules =
  let obj_map =
    Modules.obj_map modules ~f:(function
      | Normal m -> m
      | Imported_from_vlib m -> m
      | Impl_of_virtual_module { intf = _; impl } -> impl)
    |> Module.Obj_map.to_list_map ~f:(fun m _ -> (Module.obj_name m, m))
    |> Module_name.Unique.Map.of_list_exn
  in
  Staged.stage
    (List.filter_map ~f:(fun m ->
         let obj_name = Module_name.Unique.of_string m in
         Module_name.Unique.Map.find obj_map obj_name))

let parse_deps_exn ~file lines =
  let invalid () =
    User_error.raise
      [ Pp.textf "unexpected dependencies output for %s:"
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

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let dep = Obj_dir.Module.dep obj_dir in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  Action_builder.lines_of (Path.build all_deps_file)
  |> Action_builder.map ~f:(Staged.unstage @@ parse_compilation_units ~modules)
  |> Action_builder.memoize (Path.Build.to_string all_deps_file)

let read_immediate_deps_of_source ~obj_dir ~modules ~file ~ml_kind unit =
  let dep = Obj_dir.Module.dep obj_dir in
  let immediate_file = dep (Immediate (unit, ml_kind)) in
  Action_builder.lines_of (Path.build immediate_file)
  |> Action_builder.map ~f:(fun lines ->
         parse_deps_exn ~file lines
         |> parse_module_names ~dir:(Obj_dir.dir obj_dir) ~unit ~modules)
  |> Action_builder.memoize (Path.Build.to_string immediate_file)

let transitive_of_immediate_rule
    ({ sctx
     ; dir
     ; obj_dir
     ; sandbox = _
     ; modules
     ; vimpl = _
     ; stdlib = _
     ; project = _
     } as md) ~ml_kind ~file unit =
  let dep = Obj_dir.Module.dep obj_dir in
  let immediate_file = dep (Immediate (unit, ml_kind)) in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  let produce_all_deps =
    let transitive_deps modules =
      let transitive_dep m =
        let ml_kind m =
          match Module.kind m with
          | Alias _ -> None
          | _ ->
            if Module.has m ~ml_kind:Intf then Some Ml_kind.Intf else Some Impl
        in
        ml_kind m
        |> Option.map ~f:(fun ml_kind ->
               Path.build (dep (Transitive (m, ml_kind))))
      in
      List.filter_map modules ~f:transitive_dep
    in
    let open Action_builder.O in
    let paths =
      let+ lines = Action_builder.lines_of (Path.build immediate_file) in
      let immediate_deps =
        parse_deps_exn ~file lines
        |> parse_module_names ~dir:md.dir ~unit ~modules
      in
      ( transitive_deps immediate_deps
      , List.map immediate_deps ~f:(fun m ->
            Module.obj_name m |> Module_name.Unique.to_string) )
    in
    Action_builder.with_file_targets ~file_targets:[ all_deps_file ]
      (let+ sources, extras =
         Action_builder.dyn_paths
           (let+ sources, extras = paths in
            ((sources, extras), sources))
       in
       Action.Merge_files_into (sources, extras, all_deps_file))
  in
  Action_builder.With_targets.map ~f:Action.Full.make produce_all_deps
  |> Super_context.add_rule sctx ~dir

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
