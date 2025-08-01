open Import
open Memo.O

module Merge_files_into = struct
  module Spec = struct
    type ('src, 'dst) t =
      { transitive : 'src list
      ; immediate : Module_name.Unique.t list
      ; target : 'dst
      }

    let name = "merge_files_into"
    let version = 2
    let is_useful_to ~memoize:_ = true

    let bimap t path target =
      { t with transitive = List.map t.transitive ~f:path; target = target t.target }
    ;;

    let encode
          (type src dst)
          ({ transitive; immediate; target } : (src, dst) t)
          (input : src -> Sexp.t)
          (output : dst -> Sexp.t)
      : Sexp.t
      =
      List
        [ List (List.map transitive ~f:input)
        ; List
            (List.map ~f:(fun s -> Sexp.Atom (Module_name.Unique.to_string s)) immediate)
        ; output target
        ]
    ;;

    let action { transitive; immediate; target } ~ectx:_ ~eenv:_ =
      Async.async (fun () ->
        List.fold_left
          transitive
          ~init:(Module_name.Unique.Set.of_list immediate)
          ~f:(fun set source_path ->
            Io.lines_of_file source_path
            |> Module_name.Unique.Set.of_list_map ~f:Module_name.Unique.of_string
            |> Module_name.Unique.Set.union set)
        |> Module_name.Unique.Set.to_list_map ~f:Module_name.Unique.to_string
        |> Io.write_lines (Path.build target))
    ;;
  end

  module Action = Action_ext.Make (Spec)

  let action ~transitive ~immediate ~target =
    Action.action { transitive; immediate; target }
  ;;
end

let parse_module_names ~dir ~(unit : Module.t) ~modules words =
  List.concat_map words ~f:(fun m ->
    let m = Module_name.of_string m in
    match Modules.With_vlib.find_dep modules ~of_:unit m with
    | Ok s -> s
    | Error `Parent_cycle ->
      User_error.raise
        [ Pp.textf
            "Module %s in directory %s depends on %s."
            (Module_name.to_string (Module.name unit))
            (Path.to_string_maybe_quoted (Path.build dir))
            (Module_name.to_string m)
        ; Pp.textf "This doesn't make sense to me."
        ; Pp.nop
        ; Pp.textf
            "%s is the main module of the library and is the only module exposed outside \
             of the library. Consequently, it should be the one depending on all the \
             other modules in the library."
            (Module_name.to_string m)
        ])
;;

let parse_compilation_units ~modules =
  let obj_map = Modules.With_vlib.obj_map modules in
  List.filter_map ~f:(fun m ->
    let obj_name = Module_name.Unique.of_string m in
    Module_name.Unique.Map.find obj_map obj_name
    |> Option.map ~f:Modules.Sourced_module.to_module)
;;

let parse_deps_exn =
  let invalid file lines =
    User_error.raise
      [ Pp.textf
          "ocamldep returned unexpected output for %s:"
          (Path.to_string_maybe_quoted file)
      ; Pp.vbox
          (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
             Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
      ]
  in
  fun ~file lines ->
    match lines with
    | [] | _ :: _ :: _ -> invalid file lines
    | [ line ] ->
      (match String.lsplit2 line ~on:':' with
       | None -> invalid file lines
       | Some (basename, deps) ->
         let basename = Filename.basename basename in
         if basename <> Path.basename file then invalid file lines;
         String.extract_blank_separated_words deps)
;;

let transitive_deps =
  let transive_dep obj_dir m =
    (match Module.kind m with
     | Alias _ -> None
     | _ -> if Module.has m ~ml_kind:Intf then Some Ml_kind.Intf else Some Impl)
    |> Option.map ~f:(fun ml_kind ->
      Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind))
      |> Option.value_exn (* we already checked if it's an alias module *)
      |> Path.build)
  in
  fun obj_dir modules -> List.filter_map modules ~f:(transive_dep obj_dir)
;;

let deps_of ~sandbox ~modules ~sctx ~dir ~obj_dir ~ml_kind unit =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir in
  let ocamldep_output = dep (Immediate (unit, ml_kind)) |> Option.value_exn in
  let* () =
    let context = Super_context.context sctx in
    let ocamldep =
      (let+ ocaml = Context.ocaml context in
       ocaml.ocamldep)
      |> Action_builder.of_memo
    in
    Super_context.add_rule
      sctx
      ~dir
      (let open Action_builder.With_targets.O in
       let flags, sandbox =
         Module.pp_flags unit |> Option.value ~default:(Action_builder.return [], sandbox)
       in
       Command.run_dyn_prog
         ocamldep
         ~dir:(Path.build (Context.build_dir context))
         ~stdout_to:ocamldep_output
         [ A "-modules"
         ; Command.Args.dyn flags
         ; Command.Ml_kind.flag ml_kind
         ; Dep (Module.File.path source)
         ]
       >>| Action.Full.add_sandbox sandbox)
  in
  let all_deps_file = dep (Transitive (unit, ml_kind)) |> Option.value_exn in
  let+ () =
    let produce_all_deps =
      let open Action_builder.O in
      (let+ transitive, immediate =
         (let+ immediate_deps =
            Path.build ocamldep_output
            |> Action_builder.lines_of
            >>| parse_deps_exn ~file:(Module.File.path source)
            >>| parse_module_names ~dir ~unit ~modules
          in
          let transitive_deps = transitive_deps obj_dir immediate_deps in
          let immediate_deps = List.map immediate_deps ~f:Module.obj_name in
          (transitive_deps, immediate_deps), transitive_deps)
         |> Action_builder.dyn_paths
       in
       Merge_files_into.action ~transitive ~immediate ~target:all_deps_file)
      |> Action_builder.with_file_targets ~file_targets:[ all_deps_file ]
    in
    Action_builder.With_targets.map ~f:Action.Full.make produce_all_deps
    |> Super_context.add_rule sctx ~dir
  in
  let all_deps_file = Path.build all_deps_file in
  Action_builder.lines_of all_deps_file
  |> Action_builder.map ~f:(parse_compilation_units ~modules)
  |> Action_builder.memoize (Path.to_string all_deps_file)
;;

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let all_deps_file =
    Obj_dir.Module.dep obj_dir (Transitive (unit, ml_kind)) |> Option.value_exn
  in
  Action_builder.lines_of (Path.build all_deps_file)
  |> Action_builder.map ~f:(parse_compilation_units ~modules)
  |> Action_builder.memoize (Path.Build.to_string all_deps_file)
;;

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let ocamldep_output =
      Obj_dir.Module.dep obj_dir (Immediate (unit, ml_kind)) |> Option.value_exn
    in
    Action_builder.lines_of (Path.build ocamldep_output)
    |> Action_builder.map ~f:(fun lines ->
      parse_deps_exn ~file:(Module.File.path source) lines
      |> parse_module_names ~dir:(Obj_dir.dir obj_dir) ~unit ~modules)
    |> Action_builder.memoize (Path.Build.to_string ocamldep_output)
;;
