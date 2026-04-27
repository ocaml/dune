open Import

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

let ocamldep_action ~sandbox ~sctx ~dir ~ml_kind unit =
  let context = Super_context.context sctx in
  let flags, sandbox =
    Module.pp_flags unit |> Option.value ~default:(Action_builder.return [], sandbox)
  in
  let open Action_builder.O in
  let* ocamldep =
    let+ ocaml = Action_builder.of_memo (Context.ocaml context) in
    ocaml.ocamldep
  in
  let+ action =
    let source = Option.value_exn (Module.source unit ~ml_kind) in
    Command.run'
      ~dir:(Path.build (Context.build_dir context))
      ocamldep
      [ A "-modules"
      ; Command.Args.dyn flags
      ; Command.Ml_kind.flag ml_kind
      ; Dep (Module.File.path source)
      ]
  and+ env =
    (* CR-someday rgrinberg: consider getting rid of this *)
    Action_builder.of_memo
      (let open Memo.O in
       Super_context.env_node sctx ~dir >>= Env_node.external_env)
  in
  { Rule.Anonymous_action.action =
      action |> Action.Full.add_env env |> Action.Full.add_sandbox sandbox
  ; loc = Loc.none
  ; dir
  ; alias = None
  }
;;

let read_immediate_deps_of ~sandbox ~sctx ~obj_dir ~modules ~ml_kind unit =
  match Module.source ~ml_kind unit with
  | None -> Action_builder.return []
  | Some source ->
    let dir = Obj_dir.dir obj_dir in
    let memo_name =
      sprintf
        "%s.%s.ocamldep"
        (Path.to_string (Module.File.path source))
        (Ml_kind.to_string ml_kind)
    in
    ocamldep_action ~sandbox ~sctx ~dir ~ml_kind unit
    |> Build_system.execute_action_stdout
    |> Memo.map ~f:(fun output ->
      String.split_lines output
      |> parse_deps_exn ~file:(Module.File.path source)
      |> parse_module_names ~dir ~unit ~modules
      |> Stdlib.( @ ) (Modules.With_vlib.implicit_deps modules ~of_:unit))
    |> Action_builder.of_memo
    |> Action_builder.memoize memo_name
;;
