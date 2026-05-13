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

let invalid_ocamldep_output file lines =
  User_error.raise
    [ Pp.textf
        "ocamldep returned unexpected output for %s:"
        (Path.to_string_maybe_quoted file)
    ; Pp.vbox
        (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
           Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
    ]
;;

let parse_deps_exn ~file lines =
  match lines with
  | [] | _ :: _ :: _ -> invalid_ocamldep_output file lines
  | [ line ] ->
    (match String.lsplit2 line ~on:':' with
     | None -> invalid_ocamldep_output file lines
     | Some (basename, deps) ->
       let basename = Filename.basename basename in
       if basename <> Path.basename file then invalid_ocamldep_output file lines;
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

(* Top-level cache per (source path, ml_kind). Without it, each caller's
   [Action_builder.memoize] cell has a different digest (pp-flags closure
   identity varies), causing ocamldep to run multiply per source.

   The key omits [sctx], [obj_dir], and [pp_flags], but suffices because
   [Module.File.path] is build-dir-qualified for ocamldep'd modules (see
   [ml_sources.ml]'s [modules_of_files] and Melange/OxCaml equivalents):
   a build-qualified path uniquely determines its workspace context and
   stanza, and therefore all closure inputs. If that invariant ever
   stops holding, scope the cache per [Super_context]. *)
module Cache_key = struct
  type t =
    { source : Path.t
    ; ml_kind : Ml_kind.t
    }

  let equal = Poly.equal
  let hash = Poly.hash

  let to_dyn { source; ml_kind } =
    Dyn.record [ "source", Path.to_dyn source; "ml_kind", Ml_kind.to_dyn ml_kind ]
  ;;
end

let read_immediate_deps_words =
  let cache = Table.create (module Cache_key) 64 in
  fun ~sandbox ~sctx ~obj_dir ~ml_kind unit ->
    match Module.source ~ml_kind unit with
    | None -> Action_builder.return None
    | Some source ->
      let source_path = Module.File.path source in
      let cache_key = { Cache_key.source = source_path; ml_kind } in
      (match Table.find cache cache_key with
       | Some builder -> builder
       | None ->
         let dir = Obj_dir.dir obj_dir in
         let builder =
           ocamldep_action ~sandbox ~sctx ~dir ~ml_kind unit
           |> Build_system.execute_action_stdout
           |> Memo.map ~f:(fun output ->
             Some (String.split_lines output |> parse_deps_exn ~file:source_path))
           |> Action_builder.of_memo
           |> Action_builder.memoize "Ocamldep.read_immediate_deps_words"
         in
         Table.set cache cache_key builder;
         builder)
;;

let read_immediate_deps_of ~sandbox ~sctx ~obj_dir ~modules ~ml_kind unit =
  let open Action_builder.O in
  let+ words = read_immediate_deps_words ~sandbox ~sctx ~obj_dir ~ml_kind unit in
  match words with
  | None -> []
  | Some words ->
    let dir = Obj_dir.dir obj_dir in
    parse_module_names ~dir ~unit ~modules words
    |> List.append (Modules.With_vlib.implicit_deps modules ~of_:unit)
;;

(* Returns raw module names without resolving against the stanza's module set.
   Preserves references to external libraries, which [parse_module_names] would
   discard. Used for per-module inter-library dependency filtering (#4572). *)
let read_immediate_deps_raw_of ~sandbox ~sctx ~obj_dir ~ml_kind unit =
  let open Action_builder.O in
  let+ words = read_immediate_deps_words ~sandbox ~sctx ~obj_dir ~ml_kind unit in
  match words with
  | None -> Module_name.Set.empty
  | Some words -> Module_name.Set.of_list_map words ~f:Module_name.of_checked_string
;;
