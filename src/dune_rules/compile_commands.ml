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

let build_c_command ~sctx ~dir ~expander ~include_flags (src : Foreign.Source.t) ~ext_obj =
  let open Action_builder.O in
  let+ ocaml = Action_builder.of_memo (Context.ocaml (Super_context.context sctx))
  and+ args =
    (* Expand the command args to strings, discarding file-level deps (header
       files, include directories). Flag values flow through Memo, so changes
       to dune files or compiler config still invalidate this rule. *)
    Foreign_rules.c_compile_args ~sctx ~dir ~expander ~src ~include_flags
    |> Command.expand_no_targets ~dir:(Path.build dir)
    |> Action_builder.evaluate_and_collect_deps
    |> Action_builder.of_memo
    >>| fst
    >>| Appendable_list.to_list
  in
  let src_relative =
    Foreign.Source.path src |> Path.Build.basename |> Path.Local.of_string
  in
  { directory = Path.build dir
  ; file = src_relative
  ; arguments =
      List.concat
        [ [ Ocaml_config.c_compiler ocaml.ocaml_config ]
        ; args
        ; (let dst =
             Foreign.Source.object_name src ^ Filename.Extension.to_string ext_obj
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
  Foreign.Sources.to_list_map foreign_sources ~f:(fun _ (_, src) ->
    let include_flags =
      Foreign_rules.build_include_flags ~sctx ~dir ~expander ~dir_contents ~requires ~src
    in
    build_c_command ~sctx ~dir ~expander ~include_flags src ~ext_obj)
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

(* Collect all compile command entries from the workspace *)
let collect_entries sctx =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let* dune_files = Dune_load.dune_files (Context.name ctx) in
  Dune_file.fold_static_stanzas dune_files ~init:[] ~f:(fun dune_file stanza acc ->
    let dir =
      Path.Build.append_source (Context.build_dir ctx) (Dune_file.dir dune_file)
    in
    (let* expander = Super_context.expander sctx ~dir
     and* dir_contents = Dir_contents.get sctx ~dir
     and* scope = Scope.DB.find_by_dir dir in
     let* foreign_sources = Dir_contents.foreign_sources dir_contents in
     match Stanza.repr stanza with
     | Library.T lib when Buildable.has_foreign_stubs lib.buildable ->
       Foreign_sources.for_lib_opt foreign_sources ~name:(Library.best_name lib)
       |> (function
        | None -> Memo.return None
        | Some sources ->
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
    :: acc)
  |> Memo.all_concurrently
  >>| List.filter_map ~f:Fun.id
;;

let gen_rules sctx =
  let build_dir = Super_context.context sctx |> Context.build_dir in
  let open Memo.O in
  let* project = Dune_load.find_project ~dir:build_dir in
  if Dune_project.dune_version project < (3, 23)
  then Memo.return ()
  else
    let* entry_builders = collect_entries sctx in
    if List.is_empty entry_builders
    then Memo.return ()
    else (
      let gen_path = Path.Build.relative build_dir filename in
      let mode = Rule.Mode.Promote { lifetime = Until_clean; into = None; only = None } in
      let* () =
        Action_builder.write_file_dyn
          gen_path
          (let open Action_builder.O in
           let+ entries = Action_builder.all entry_builders >>| List.concat in
           Json.to_string (`List (List.map entries ~f:entry_to_json)))
        |> Super_context.add_rule sctx ~mode ~dir:build_dir
      in
      Rules.Produce.Alias.add_deps
        (Alias.make Alias0.check ~dir:build_dir)
        (Action_builder.path (Path.build gen_path)))
;;
