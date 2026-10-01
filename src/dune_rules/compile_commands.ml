open Import

let filename = "compile_commands.json"

(* A single entry in compile_commands.json *)
type entry =
  { directory : Path.t
  ; file : Path.Local.t
  ; arguments : string list
  }

let entry_to_json { directory; file; arguments } : Json.t =
  `Assoc
    [ "directory", `String (Path.to_string directory)
    ; "file", `String (Path.Local.to_string file)
    ; "arguments", `List (List.map arguments ~f:(fun arg -> `String arg))
    ]
;;

let build_c_command
      ~sctx
      ~dir
      ~expander
      ~include_flags
      ~loc
      (src : Foreign.Source.t)
      ~ext_obj
  =
  let open Action_builder.O in
  let+ ocaml = Action_builder.of_memo (Context.ocaml (Super_context.context sctx))
  and+ args =
    (* Expand the command args to strings, discarding file-level deps (header
       files, include directories). Flag values flow through Memo, so changes
       to dune files or compiler config still invalidate this rule. *)
    Foreign_rules.c_compile_args ~sctx ~dir ~expander ~loc ~src ~include_flags
    |> Command.expand_no_targets ~dir:(Path.build dir)
    |> Action_builder.evaluate_and_collect_deps
    |> Action_builder.of_memo
    >>| fst
    >>| Appendable_list.to_list
  in
  let src_relative =
    Foreign.Source.path src
    |> Path.Build.basename
    |> Path.Local.relative_fname Path.Local.root
  in
  { directory = Path.build dir
  ; file = src_relative
  ; arguments =
      List.concat
        [ [ Ocaml_config.c_compiler ocaml.ocaml_config ]
        ; args
        ; (let dst =
             Filename.to_string (Foreign.Source.object_name src)
             ^ Filename.Extension.to_string ext_obj
           in
           match ocaml.lib_config.ccomp_type with
           | Msvc -> [ "/Fo" ^ dst ]
           | Cc | Other _ -> [ "-o"; dst ])
        ; [ "-c"; Path.Local.to_string src_relative ]
        ]
  }
;;

(* Collect entries from foreign sources in a directory.
   [requires] is the list of library dependencies for include paths. *)
let collect_from_foreign_sources
      ~sctx
      ~dir
      ~expander
      ~dir_contents
      ~requires
      foreign_sources
  =
  let open Action_builder.O in
  let* ext_obj =
    let+ ocaml = Action_builder.of_memo (Context.ocaml (Super_context.context sctx)) in
    ocaml.lib_config.ext_obj
  in
  Foreign.Sources.to_list_map foreign_sources ~f:(fun _ (loc, src) ->
    let include_flags =
      Foreign_rules.build_include_flags ~sctx ~dir ~expander ~dir_contents ~requires ~src
    in
    build_c_command ~sctx ~dir ~expander ~include_flags ~loc src ~ext_obj)
  |> Action_builder.all
;;

(* Get library compile requirements for include paths *)
let get_lib_requires ~dir ~scope (lib : Library.t) =
  let open Memo.O in
  Lib.DB.get_compile_info
    (Scope.libs scope)
    (Local (Library.to_lib_id ~src_dir:(Path.Build.drop_build_context_exn dir) lib))
    ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
  >>| snd
  >>= Lib.Compile.direct_requires ~for_:Ocaml
;;

(* Collect compile command entries from the selected foreign stanzas. *)
let collect_entries sctx stanzas =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  Memo.parallel_map stanzas ~f:(fun (dune_file, stanza) ->
    let dir =
      Path.Build.append_source (Context.build_dir ctx) (Dune_file.dir dune_file)
    in
    let* expander = Super_context.expander sctx ~dir
    and* dir_contents = Dir_contents.get sctx ~dir in
    let* foreign_sources = Dir_contents.foreign_sources dir_contents in
    match Stanza.repr stanza with
    | Library.T lib when Buildable.has_foreign_stubs lib.buildable ->
      Foreign_sources.for_lib_opt foreign_sources ~name:(Library.best_name lib)
      |> (function
       | None -> Memo.return None
       | Some sources ->
         let* scope = Scope.DB.find_by_dir dir in
         get_lib_requires ~dir ~scope lib
         >>| fun requires ->
         Some
           (collect_from_foreign_sources
              ~sctx
              ~dir
              ~expander
              ~dir_contents
              ~requires
              sources))
    | (Executables.T exes | Tests.T { exes; _ })
      when Buildable.has_foreign_stubs exes.buildable ->
      let requires = Resolve.return [] in
      Foreign_sources.for_exes_opt
        foreign_sources
        ~first_exe:(snd (Nonempty_list.hd exes.names))
      |> Option.map
           ~f:(collect_from_foreign_sources ~sctx ~dir ~expander ~dir_contents ~requires)
      |> Memo.return
    | Foreign_library.T lib ->
      let requires = Resolve.return [] in
      Foreign_sources.for_archive_opt foreign_sources ~archive_name:lib.archive_name
      |> Option.map
           ~f:(collect_from_foreign_sources ~sctx ~dir ~expander ~dir_contents ~requires)
      |> Memo.return
    | _ -> Memo.return None)
  >>| List.filter_map ~f:Fun.id
;;

let gen_rules sctx ~rules =
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let open Memo.O in
  let* project = Dune_load.find_project ~dir:build_dir in
  if Dune_project.dune_version project < (3, 23)
  then Memo.return ()
  else
    let* dune_files = Dune_load.dune_files (Context.name ctx) in
    let foreign_stanzas =
      Dune_file.fold_static_stanzas dune_files ~init:[] ~f:(fun dune_file stanza acc ->
        match Stanza.repr stanza with
        | Foreign_library.T _ -> (dune_file, stanza) :: acc
        | Library.T { buildable; _ }
        | Executables.T { buildable; _ }
        | Tests.T { exes = { buildable; _ }; _ }
          when Buildable.has_foreign_stubs buildable -> (dune_file, stanza) :: acc
        | _ -> acc)
    in
    let has_existing_rule =
      let { Rules.Dir_rules.rules; _ } =
        Rules.find rules (Path.build build_dir) |> Rules.Dir_rules.consume
      in
      let filename = Filename.of_string_exn filename in
      List.exists rules ~f:(fun { Rule.targets = { files; dirs; _ }; _ } ->
        Filename.Set.mem files filename || Filename.Set.mem dirs filename)
    in
    if List.is_empty foreign_stanzas || has_existing_rule
    then Memo.return ()
    else (
      let gen_path = Path.Build.relative build_dir filename in
      let mode = Rule.Mode.Promote { lifetime = Until_clean; into = None; only = None } in
      let* () =
        Action_builder.write_file_dyn
          gen_path
          (let open Action_builder.O in
           (* Discovering copied sources can depend on these root rules. *)
           let* entry_builders =
             Action_builder.of_memo (collect_entries sctx foreign_stanzas)
           in
           match entry_builders with
           | [] ->
             (* Identical contents prevent promotion from overwriting the
                source or scheduling it for deletion on clean. *)
             let source = Path.source (Path.Build.drop_build_context_exn gen_path) in
             Action_builder.if_file_exists
               source
               ~then_:(Action_builder.contents source)
               ~else_:(Action_builder.return "[]")
           | _ ->
             let+ entries = Action_builder.all entry_builders >>| List.concat in
             Json.to_string (`List (List.map entries ~f:entry_to_json)))
        |> Super_context.add_rule sctx ~mode ~dir:build_dir
      in
      Rules.Produce.Alias.add_deps
        (Alias.make Alias0.check ~dir:build_dir)
        (Action_builder.path (Path.build gen_path)))
;;
