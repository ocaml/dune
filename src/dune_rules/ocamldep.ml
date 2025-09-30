open Import
open Memo.O

let parse_module_names ~dir ~(unit : Module.t) ~modules words =
  List.concat_map words ~f:(fun m ->
    let m = Module_name.of_checked_string m in
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

module Top_closure = Top_closure.Make (Module_name.Unique.Set) (Action_builder)

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit =
  let obj_map = Modules.With_vlib.obj_map modules in
  match Module_name.Unique.Map.find obj_map (Module.obj_name unit) with
  | Some (Modules.Sourced_module.Imported_from_vlib _) -> Action_builder.return []
  | _ ->
    (match Module.source ~ml_kind unit with
     | None -> Action_builder.return []
     | Some source ->
       (match Obj_dir.Module.dep obj_dir ~for_ (Immediate (unit, ml_kind)) with
        | None -> Action_builder.return []
        | Some ocamldep_output ->
          let implicit_deps = Modules.With_vlib.implicit_deps modules ~of_:unit in
          Action_builder.lines_of (Path.build ocamldep_output)
          |> Action_builder.map ~f:(fun lines ->
            parse_deps_exn ~file:(Module.File.path source) lines
            |> parse_module_names ~dir:(Obj_dir.dir obj_dir) ~unit ~modules
            |> Stdlib.( @ ) implicit_deps)
          |> Action_builder.memoize (Path.Build.to_string ocamldep_output)))
;;

let dep_kind m =
  match Module.kind m with
  | Root | Alias _ -> None
  | _ -> if Module.has m ~ml_kind:Intf then Some Ml_kind.Intf else Some Impl
;;

let intf_deps_of ~obj_dir ~modules ~for_ unit =
  match dep_kind unit with
  | None -> Action_builder.return []
  | Some ml_kind -> read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit
;;

let compute_transitive_deps ~obj_dir ~modules ~for_ ~unit units =
  Top_closure.top_closure
    units
    ~key:Module.obj_name
    ~deps:(intf_deps_of ~obj_dir ~modules ~for_)
  |> Action_builder.map ~f:(function
    | Ok modules -> modules
    | Error cycle ->
      User_error.raise
        [ Pp.textf
            "dependency cycle involving module %s:"
            (Module_name.to_string (Module.name unit))
        ; Pp.chain cycle ~f:(fun m -> Pp.verbatim (Module_name.to_string (Module.name m)))
        ])
;;

let all_deps ~obj_dir ~modules ~ml_kind ~for_ unit =
  let open Action_builder.O in
  let* immediate = read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit in
  let+ transitive_deps =
    compute_transitive_deps ~obj_dir ~modules ~for_ ~unit immediate
  in
  List.sort_uniq (immediate @ transitive_deps) ~compare:(fun x y ->
    Module_name.Unique.compare (Module.obj_name x) (Module.obj_name y))
;;

let deps_of ~sandbox ~modules ~sctx ~dir ~obj_dir ~ml_kind ~for_ unit =
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let dep = Obj_dir.Module.dep obj_dir ~for_ in
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
  Memo.return (all_deps ~obj_dir ~modules ~ml_kind ~for_ unit)
;;

let read_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit =
  let key =
    Obj_dir.Module.dep obj_dir ~for_ (Immediate (unit, ml_kind))
    |> Option.value_exn
    |> Path.Build.to_string
  in
  all_deps ~obj_dir ~modules ~ml_kind ~for_ unit |> Action_builder.memoize key
;;
