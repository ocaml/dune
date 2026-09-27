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
    match Foreign.Source.language src with
    | Asm -> None
    | C | Cxx ->
      let include_flags =
        Foreign_rules.build_include_flags
          ~sctx
          ~dir
          ~expander
          ~dir_contents
          ~requires
          ~src
      in
      Some (build_c_command ~sctx ~dir ~expander ~include_flags ~loc src ~ext_obj))
  |> List.filter_map ~f:Fun.id
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

let enabled_stanzas =
  let memo =
    Memo.create
      "compile-commands-enabled-stanzas"
      ~input:(module Super_context.As_memo_key)
      (fun sctx ->
         let open Memo.O in
         let ctx = Super_context.context sctx in
         let* dune_files = Dune_load.dune_files (Context.name ctx) in
         Dune_file.fold_static_stanzas dune_files ~init:[] ~f:(fun dune_file stanza acc ->
           let enabled_if =
             match Stanza.repr stanza with
             | Foreign_library.T lib -> Some lib.enabled_if
             | Library.T lib when Buildable.has_foreign_stubs lib.buildable ->
               Some lib.enabled_if
             | (Executables.T exes | Tests.T { exes; _ })
               when Buildable.has_foreign_stubs exes.buildable -> Some exes.enabled_if
             | _ -> None
           in
           match enabled_if with
           | None -> acc
           | Some enabled_if -> (dune_file, stanza, enabled_if) :: acc)
         |> Memo.parallel_map ~f:(fun (dune_file, stanza, enabled_if) ->
           let dir = Dune_file.output_dir dune_file in
           let* expander = Super_context.expander sctx ~dir in
           let+ enabled = Expander.eval_blang expander enabled_if in
           Option.some_if enabled (dune_file, stanza))
         >>| List.filter_opt)
  in
  fun sctx -> Memo.exec memo sctx
;;

(* Discover sources only when the internal database rule is built. *)
let collect_entries sctx stanzas =
  let open Memo.O in
  Memo.parallel_map stanzas ~f:(fun (dune_file, stanza) ->
    let dir = Dune_file.output_dir dune_file in
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

let is_supported sctx =
  let ctx = Super_context.context sctx in
  if not (Context.merlin ctx)
  then Memo.return false
  else
    let open Memo.O in
    let+ project = Dune_load.find_project ~dir:(Context.build_dir ctx) in
    Dune_project.dune_version project >= (3, 23)
;;

let setup_aliases sctx =
  let open Memo.O in
  let* supported = is_supported sctx in
  if not supported
  then Memo.return ()
  else (
    let build_dir = Context.build_dir (Super_context.context sctx) in
    let dependency =
      let open Action_builder.O in
      let* stanzas = Action_builder.of_memo (enabled_stanzas sctx) in
      if List.is_empty stanzas
      then Action_builder.return ()
      else (
        let user_path = Path.Build.relative build_dir filename in
        (* A source copy of a previously promoted database is not an override. *)
        let selector =
          File_selector.of_predicate_lang
            ~dir:(Path.build build_dir)
            ~only_generated_files:true
            (Predicate_lang.Glob.of_string_set (String.Set.singleton filename))
        in
        let* generated = Action_builder.of_memo (Build_system.eval_pred selector)
        and* target_type =
          Action_builder.of_memo (Load_rules.is_target (Path.build user_path))
        in
        let has_user_target =
          match target_type with
          | Yes Directory -> true
          | No | Yes File | Under_directory_target_so_cannot_say ->
            not (Filename_set.is_empty generated)
        in
        let path =
          if has_user_target
          then user_path
          else (
            let dir = Path.Build.relative_fname build_dir Filename.dune_dir_basename in
            Path.Build.relative dir filename)
        in
        Action_builder.path (Path.build path))
    in
    Memo.parallel_iter
      [ Alias0.check; Alias.Name.of_string "compile_commands" ]
      ~f:(fun name ->
        Rules.Produce.Alias.add_deps (Alias.make name ~dir:build_dir) dependency))
;;

(* Configurator rules can load this directory before the super-context is ready. *)
let gen_rules sctx ~dir =
  let path = Path.Build.relative dir filename in
  let mode =
    Rule.Mode.Promote
      { lifetime = Until_clean; into = Some { loc = Loc.none; dir = ".." }; only = None }
  in
  (let open Action_builder.O in
   let* sctx = Action_builder.of_memo sctx in
   let* supported = Action_builder.of_memo (is_supported sctx) in
   if not supported
   then User_error.raise [ Pp.text "Compilation database generation is unavailable." ];
   let* stanzas = Action_builder.of_memo (enabled_stanzas sctx) in
   if List.is_empty stanzas
   then
     User_error.raise [ Pp.text "No enabled foreign stanzas for compilation database." ];
   let* entry_builders = Action_builder.of_memo (collect_entries sctx stanzas) in
   let+ entries = Action_builder.all entry_builders >>| List.concat in
   Json.to_string (`List (List.map entries ~f:entry_to_json))
   |> Action.write_file path
   |> Action.Full.make)
  |> Rule.make ~mode ~targets:(Targets.File.create path)
  |> Rules.Produce.rule
;;
