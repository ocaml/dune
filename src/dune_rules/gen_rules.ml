open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let install_stanza_rules ~ctx_dir ~expander (install_conf : Install_conf.t) =
  let action =
    (* XXX we're evaluating these stanzas here and [Install_rules]. Seems a bit
       sad to do that *)
    let files_and_dirs =
      let expand_str = Expander.No_deps.expand_str expander in
      let+ files_expanded =
        Install_entry.File.to_file_bindings_expanded
          install_conf.files
          ~expand_str
          ~dir:ctx_dir
      and+ dirs_expanded =
        Install_entry.Dir.to_file_bindings_expanded
          install_conf.dirs
          ~expand_str
          ~dir:ctx_dir
          ~relative_dst_path_starts_with_parent_error_when:`Deprecation_warning_from_3_11
      in
      List.map (files_expanded @ dirs_expanded) ~f:(fun fb ->
        File_binding.Expanded.src fb |> Path.build)
    in
    let open Action_builder.O in
    Action_builder.of_memo files_and_dirs >>= Action_builder.paths
  in
  Rules.Produce.Alias.add_deps (Alias.make Alias0.all ~dir:ctx_dir) action
;;

module For_stanza : sig
  type ('merlin, 'cctx, 'js, 'source_dirs) t =
    { merlin : 'merlin
    ; cctx : 'cctx
    ; js : 'js
    ; source_dirs : 'source_dirs
    }

  val of_stanzas
    :  Stanza.t list
    -> cctxs:(Loc.t * Compilation_context.t) list
    -> sctx:Super_context.t
    -> src_dir:Path.Source.t
    -> ctx_dir:Path.Build.t
    -> scope:Scope.t
    -> dir_contents:Dir_contents.t
    -> expander:Expander.t
    -> ( Merlin.t list
         , (Loc.t * Compilation_context.t) list
         , Path.Build.t list
         , Path.Source.t list )
         t
         Memo.t
end = struct
  type ('merlin, 'cctx, 'js, 'source_dirs) t =
    { merlin : 'merlin
    ; cctx : 'cctx
    ; js : 'js
    ; source_dirs : 'source_dirs
    }

  let empty_none = { merlin = None; cctx = None; js = None; source_dirs = None }
  let empty_list = { merlin = []; cctx = []; js = []; source_dirs = [] }

  let cons_maybe hd_o tl =
    match hd_o with
    | Some hd -> hd :: tl
    | None -> tl
  ;;

  let cons acc x =
    { merlin = cons_maybe x.merlin acc.merlin
    ; cctx = cons_maybe x.cctx acc.cctx
    ; source_dirs = cons_maybe x.source_dirs acc.source_dirs
    ; js =
        (match x.js with
         | None -> acc.js
         | Some js -> List.rev_append acc.js js)
    }
  ;;

  let rev t =
    { t with
      merlin = List.rev t.merlin
    ; cctx = List.rev t.cctx
    ; source_dirs = List.rev t.source_dirs
    }
  ;;

  let if_available f = function
    | false -> Memo.return empty_none
    | true -> f ()
  ;;

  let with_cctx_merlin ~loc (cctx, merlin) =
    { empty_none with merlin = Some merlin; cctx = Some (loc, cctx) }
  ;;

  let if_available_buildable ~loc f = function
    | false -> Memo.return empty_none
    | true -> f () >>| with_cctx_merlin ~loc
  ;;

  (* XXX it's weird how we're handling some [enabled_if] here, and others are
     being handled directly in the stanza. *)
  let of_stanza stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander =
    let dir = ctx_dir in
    let toplevel_setup = Toplevel.Stanza.setup in
    let open Dune_file in
    match Stanza.repr stanza with
    | Toplevel_stanza.T toplevel ->
      let+ () = toplevel_setup ~sctx ~dir ~toplevel in
      empty_none
    | Library.T lib ->
      (* XXX why are we setting up private doc rules for disabled libraries? *)
      let* () = Odoc.setup_private_library_doc_alias sctx ~scope ~dir:ctx_dir lib
      and+ enabled_if =
        Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
      in
      if_available_buildable
        ~loc:lib.buildable.loc
        (fun () -> Lib_rules.rules lib ~sctx ~dir ~scope ~dir_contents ~expander)
        enabled_if
    | Foreign.Library.T lib ->
      let+ () = Lib_rules.foreign_rules lib ~sctx ~dir ~dir_contents ~expander in
      empty_none
    | Executables.T exes ->
      Expander.eval_blang expander exes.enabled_if
      >>= if_available (fun () ->
        let+ () =
          Memo.Option.iter exes.install_conf ~f:(install_stanza_rules ~expander ~ctx_dir)
        and+ cctx_merlin =
          Exe_rules.rules exes ~sctx ~dir ~scope ~expander ~dir_contents
        in
        { (with_cctx_merlin ~loc:exes.buildable.loc cctx_merlin) with
          js =
            Some
              (List.map exes.names ~f:(fun (_, exe) ->
                 Path.Build.relative dir (exe ^ Js_of_ocaml.Ext.exe)))
        })
    | Alias_conf.T alias ->
      let+ () = Simple_rules.alias sctx alias ~dir ~expander in
      empty_none
    | Tests.T tests ->
      Expander.eval_blang expander tests.build_if
      >>= if_available_buildable ~loc:tests.exes.buildable.loc (fun () ->
        Test_rules.rules tests ~sctx ~dir ~scope ~expander ~dir_contents)
    | Copy_files.T { files = glob; _ } ->
      let+ source_dirs =
        let+ src_glob = Expander.No_deps.expand_str expander glob in
        if Filename.is_relative src_glob
        then (
          match
            let error_loc = String_with_vars.loc glob in
            Path.relative (Path.source src_dir) src_glob ~error_loc
          with
          | In_source_tree s -> Some (Path.Source.parent_exn s)
          | In_build_dir _ | External _ -> None)
        else None
      in
      { empty_none with source_dirs }
    | Install_conf.T i ->
      let+ () = install_stanza_rules ~ctx_dir ~expander i in
      empty_none
    | Plugin.T p ->
      let+ () = Plugin_rules.setup_rules ~sctx ~dir p in
      empty_none
    | Cinaps.T cinaps ->
      let+ () = Cinaps.gen_rules sctx cinaps ~dir ~scope in
      empty_none
    | Mdx.T mdx ->
      Expander.eval_blang expander (Mdx.enabled_if mdx)
      >>= if_available (fun () ->
        let+ () = Mdx.gen_rules ~sctx ~dir ~scope ~expander mdx in
        empty_none)
    | Melange_stanzas.Emit.T mel ->
      Expander.eval_blang expander mel.enabled_if
      >>= if_available_buildable ~loc:mel.loc (fun () ->
        Melange_rules.setup_emit_cmj_rules ~dir_contents ~dir ~scope ~sctx ~expander mel)
    | _ -> Memo.return empty_none
  ;;

  let of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander =
    Memo.parallel_map
      stanzas
      ~f:(of_stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander)
    >>| List.fold_left ~init:{ empty_list with cctx = cctxs } ~f:(fun acc x -> cons acc x)
    >>| rev
  ;;
end

let define_all_alias ~dir ~project ~js_targets =
  let deps =
    let predicate =
      if Dune_project.explicit_js_mode project
      then Predicate_lang.true_
      else (
        List.iter js_targets ~f:(fun js_target ->
          assert (Path.Build.equal (Path.Build.parent_exn js_target) dir));
        Predicate_lang.not
          (Predicate_lang.Glob.of_string_set
             (String.Set.of_list_map js_targets ~f:Path.Build.basename)))
    in
    let only_generated_files = Dune_project.dune_version project >= (3, 0) in
    File_selector.of_predicate_lang ~dir:(Path.build dir) ~only_generated_files predicate
    |> Action_builder.paths_matching_unit ~loc:Loc.none
  in
  Rules.Produce.Alias.add_deps (Alias.make Alias0.all ~dir) deps
;;

let gen_rules_for_stanzas
  sctx
  dir_contents
  cctxs
  expander
  { Dune_file.dir = src_dir; stanzas; project }
  ~dir:ctx_dir
  =
  let* { For_stanza.merlin = merlins; cctx = cctxs; js = js_targets; source_dirs } =
    let* scope = Scope.DB.find_by_dir ctx_dir in
    For_stanza.of_stanzas
      stanzas
      ~cctxs
      ~sctx
      ~src_dir
      ~ctx_dir
      ~scope
      ~dir_contents
      ~expander
  in
  let+ () =
    let more_src_dirs =
      Merlin.more_src_dirs dir_contents ~source_dirs:(src_dir :: source_dirs)
    in
    Memo.parallel_iter
      merlins
      ~f:(Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~expander)
  and+ () =
    Memo.parallel_iter stanzas ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Menhir_stanza.T m ->
        Expander.eval_blang expander m.enabled_if
        >>= (function
         | false -> Memo.return ()
         | true ->
           let* ml_sources = Dir_contents.ocaml dir_contents in
           (match
              let base_path =
                match Ml_sources.include_subdirs ml_sources with
                | Include Unqualified | No -> []
                | Include Qualified ->
                  Path.Local.descendant
                    (Path.Build.local ctx_dir)
                    ~of_:(Path.Build.local (Dir_contents.dir dir_contents))
                  |> Option.value_exn
                  |> Path.Local.explode
                  |> List.map ~f:Module_name.of_string
              in
              Menhir_rules.module_names m
              |> List.find_map ~f:(fun name ->
                let open Option.O in
                let path = base_path @ [ name ] in
                let* origin = Ml_sources.find_origin ml_sources path in
                List.find_map cctxs ~f:(fun (loc, cctx) ->
                  Option.some_if (Loc.equal loc (Ml_sources.Origin.loc origin)) cctx))
            with
            | Some cctx -> Menhir_rules.gen_rules cctx m ~dir:ctx_dir
            | None ->
              (* This happens often when passing a [-p ...] option that hides a
                 library *)
              let file_targets =
                Menhir_stanza.targets m |> List.map ~f:(Path.Build.relative ctx_dir)
              in
              Super_context.add_rule
                sctx
                ~dir:ctx_dir
                (Action_builder.fail
                   { fail =
                       (fun () ->
                         User_error.raise
                           ~loc:m.loc
                           [ Pp.text
                               "I can't determine what library/executable the files \
                                produced by this stanza are part of."
                           ])
                   }
                 |> Action_builder.with_file_targets ~file_targets)))
      | Coq_stanza.Theory.T m ->
        Expander.eval_blang expander m.enabled_if
        >>= (function
         | false -> Memo.return ()
         | true -> Coq_rules.setup_theory_rules ~sctx ~dir:ctx_dir ~dir_contents m)
      | Coq_stanza.Extraction.T m ->
        Coq_rules.setup_extraction_rules ~sctx ~dir:ctx_dir ~dir_contents m
      | Coq_stanza.Coqpp.T m -> Coq_rules.setup_coqpp_rules ~sctx ~dir:ctx_dir m
      | _ -> Memo.return ())
  and+ () = define_all_alias ~dir:ctx_dir ~project ~js_targets in
  cctxs
;;

let gen_format_and_cram_rules sctx ~expander ~dir source_dir =
  let+ () = Format_rules.setup_alias ~dir
  and+ () =
    Source_tree.Dir.cram_tests source_dir >>= Cram_rules.rules ~sctx ~expander ~dir
  in
  ()
;;

let gen_rules_source_only sctx ~dir source_dir =
  Rules.collect_unit (fun () ->
    let* sctx = sctx in
    let* expander = Super_context.expander sctx ~dir in
    let+ () = gen_format_and_cram_rules sctx ~expander ~dir source_dir
    and+ () =
      define_all_alias ~dir ~js_targets:[] ~project:(Source_tree.Dir.project source_dir)
    in
    ())
;;

let gen_rules_group_part_or_root sctx dir_contents cctxs ~source_dir ~dir
  : (Loc.t * Compilation_context.t) list Memo.t
  =
  let* expander = Super_context.expander sctx ~dir in
  let* () = gen_format_and_cram_rules sctx ~expander ~dir source_dir
  and+ stanzas =
    (* CR-soon rgrinberg: we shouldn't have to fetch the stanzas yet again *)
    Only_packages.stanzas_in_dir dir
    >>= function
    | Some d -> Memo.return (Some d)
    | None ->
      let project = Source_tree.Dir.project source_dir in
      let+ () = define_all_alias ~dir ~js_targets:[] ~project in
      None
  in
  let+ contexts =
    match stanzas with
    | None -> Memo.return []
    | Some d -> gen_rules_for_stanzas sctx dir_contents cctxs expander d ~dir
  in
  contexts
;;

(* Warn whenever [(name <name>)]) is missing from the [dune-project] file *)
let missing_project_name =
  Warning.make
    ~default:(fun version -> if version >= (2, 8) then `Enabled else `Disabled)
    ~name:"missing_project_name"
    ~since:(3, 11)
;;

(* To be called once per project, when we are generating the rules for the root
   directory of the project *)
let gen_project_rules =
  let rules sctx project =
    let* sctx = sctx in
    let+ () = Install_rules.gen_project_rules sctx project
    and+ () = Odoc.gen_project_rules sctx project
    and+ () = Odoc_new.gen_project_rules sctx project
    and+ () =
      let version = 2, 8 in
      match Dune_project.allow_approximate_merlin project with
      | None -> Memo.return ()
      | Some _ when Dune_project.dune_version project < version -> Memo.return ()
      | Some loc ->
        let+ vendored = Source_tree.is_vendored (Dune_project.root project) in
        if not vendored
        then
          Dune_lang.Syntax.Warning.deprecated_in
            ~extra_info:
              "It is useless since the Merlin configurations are not ambiguous anymore."
            loc
            Stanza.syntax
            version
            ~what:"This field"
    and+ () =
      match Dune_project.name project with
      | Named _ -> Memo.return ()
      | Anonymous _ ->
        (match
           Dune_project.dune_version project >= (2, 8)
           && Dune_project.generate_opam_files project
         with
         | false -> Memo.return ()
         | true ->
           Warning_emit.emit
             missing_project_name
             (Warning_emit.Context.project project)
             (fun () ->
                let+ () = Memo.return () in
                let loc = Loc.in_file (Path.source (Dune_project.file project)) in
                User_message.make
                  ~loc
                  [ Pp.text
                      "Project name is not specified. Add a (name <project-name>) field \
                       to your dune-project file to make sure that $ dune subst works in \
                       release or pinned builds"
                  ]))
    in
    ()
  in
  fun sctx source_dir ->
    let* () = Memo.return () in
    let project = Source_tree.Dir.project source_dir in
    match
      Path.Source.equal (Source_tree.Dir.path source_dir) (Dune_project.root project)
    with
    | false -> Memo.return Rules.empty
    | true -> Rules.collect_unit (fun () -> rules sctx project)
;;

module Automatic_subdir = struct
  (* TODO Handle all of these subdirectories at the toplevel like [.topmod] *)

  (* Sub-dirs that are automatically generated in all directories. Or rather, all
     the ones that have a corresponding source directory. *)
  type t =
    | Utop
    | Formatted
    | Bin

  let map =
    Filename.Map.of_list_exn
      [ Utop.utop_dir_basename, Utop
      ; Format_rules.formatted_dir_basename, Formatted
      ; Artifacts.bin_dir_basename, Bin
      ]
  ;;

  let of_src_dir src_dir =
    match Path.Source.basename_opt src_dir with
    | None -> None
    | Some basename -> Filename.Map.find map basename
  ;;

  let subdirs components =
    match List.last components with
    | None -> Filename.Set.of_keys map
    | Some comp ->
      if Filename.Map.mem map comp then Filename.Set.empty else Filename.Set.of_keys map
  ;;

  let gen_rules ~sctx ~dir kind =
    match kind with
    | Utop -> Utop.setup sctx ~dir:(Path.Build.parent_exn dir)
    | Formatted -> Format_rules.gen_rules sctx ~output_dir:dir
    | Bin ->
      Super_context.env_node sctx ~dir:(Path.Build.parent_exn dir)
      >>= Env_node.local_binaries
      >>= Memo.parallel_iter ~f:(fun t ->
        let loc = File_binding.Expanded.src_loc t in
        let src = Path.build (File_binding.Expanded.src t) in
        let dst = File_binding.Expanded.dst_path t ~dir in
        Super_context.add_rule sctx ~loc ~dir (Action_builder.symlink ~src ~dst))
  ;;
end

let has_rules ~dir subdirs f =
  let rules = Rules.collect_unit f in
  Memo.return
    (Gen_rules.make
       ~build_dir_only_sub_dirs:(Gen_rules.Build_only_sub_dirs.singleton ~dir subdirs)
       rules)
;;

let gen_rules_standalone_or_root sctx ~dir ~source_dir =
  let* sctx = sctx in
  let* standalone_or_root =
    Dir_contents.triage ~dir sctx
    >>| function
    | Group_part _ -> assert false
    | Standalone_or_root standalone_or_root -> standalone_or_root
  in
  let* () = Memo.Lazy.force Configurator_rules.force_files in
  let* rules' =
    Rules.collect_unit (fun () ->
      let* dir_contents = Dir_contents.Standalone_or_root.root standalone_or_root in
      let* cctxs = gen_rules_group_part_or_root sctx dir_contents [] ~source_dir ~dir in
      Dir_contents.Standalone_or_root.subdirs standalone_or_root
      >>= Memo.parallel_iter ~f:(fun dc ->
        let+ (_ : (Loc.t * Compilation_context.t) list) =
          gen_rules_group_part_or_root
            sctx
            dir_contents
            cctxs
            ~source_dir
            ~dir:(Dir_contents.dir dc)
        in
        ()))
  in
  let+ rules = Dir_contents.Standalone_or_root.rules standalone_or_root in
  Rules.union rules rules'
;;

let gen_automatic_subdir_rules sctx ~dir ~nearest_src_dir ~src_dir =
  (* There is always a source dir at the root, so we can't be at the root if
     we are in this branch *)
  match
    match nearest_src_dir with
    | None -> None
    | Some _ -> Automatic_subdir.of_src_dir src_dir
  with
  | None -> Memo.return Rules.empty
  | Some kind ->
    Rules.collect_unit (fun () ->
      let* sctx = sctx in
      Automatic_subdir.gen_rules ~sctx ~dir kind)
;;

let gen_rules_regular_directory sctx ~src_dir ~components ~dir =
  Dir_status.DB.get ~dir
  >>= function
  | Lock_dir -> Memo.return Gen_rules.no_rules
  | dir_status ->
    let+ rules =
      let* st_dir = Source_tree.find_dir src_dir in
      let* nearest_src_dir =
        match st_dir with
        | Some dir -> Memo.return (Some dir)
        | None -> Source_tree.find_dir (Path.Source.parent_exn src_dir)
      in
      let+ rules =
        let+ make_rules =
          let+ directory_targets = Dir_status.directory_targets dir_status ~dir in
          let allowed_subdirs =
            let automatic = Automatic_subdir.subdirs components in
            let toplevel =
              match components with
              | _ :: _ -> Filename.Set.empty
              | [] ->
                (* XXX sync this list with the pattern matches above. It's quite ugly
                   we need this, we should rewrite this code to avoid this. *)
                Filename.Set.of_list
                  [ ".js"; "_doc"; "_doc_new"; ".ppx"; ".dune"; ".topmod" ]
            in
            Filename.Set.union automatic toplevel
          in
          fun rules ->
            let rules =
              let+ automatic_subdir_rules =
                gen_automatic_subdir_rules sctx ~dir ~nearest_src_dir ~src_dir
              and+ project_rules =
                match st_dir with
                | None -> Memo.return Rules.empty
                | Some st_dir -> gen_project_rules sctx st_dir
              and+ rules = rules in
              Rules.union (Rules.union project_rules automatic_subdir_rules) rules
            in
            Gen_rules.rules_for ~dir ~directory_targets ~allowed_subdirs rules
        in
        match dir_status with
        | Lock_dir -> Gen_rules.rules_here Gen_rules.Rules.empty
        | Source_only source_dir ->
          gen_rules_source_only sctx ~dir source_dir |> make_rules |> Gen_rules.rules_here
        | Generated | Is_component_of_a_group_but_not_the_root _ ->
          Memo.return Rules.empty |> make_rules |> Gen_rules.redirect_to_parent
        | Standalone (source_dir, _) | Group_root { source_dir; _ } ->
          gen_rules_standalone_or_root sctx ~dir ~source_dir
          |> make_rules
          |> Gen_rules.rules_here
      in
      match Opam_create.gen_rules sctx ~dir ~nearest_src_dir ~src_dir with
      | None -> rules
      | Some opam_rules ->
        Gen_rules.map_rules rules ~f:(Gen_rules.Rules.combine_exn opam_rules)
    and+ melange_rules = Melange_rules.setup_emit_js_rules sctx ~dir in
    Gen_rules.combine melange_rules rules
;;

(* Once [gen_rules] has decided what to do with the directory, it should end
   with [has_rules] or [redirect_to_parent] *)
let gen_rules ctx sctx ~dir components : Gen_rules.result Memo.t =
  let src_dir = Path.Build.drop_build_context_exn dir in
  match components with
  | [ ".dune"; "ccomp" ] ->
    has_rules ~dir Subdir_set.empty (fun () ->
      (* Add rules for C compiler detection *)
      let* sctx = sctx in
      Cxx_rules.rules ~sctx ~dir)
  | ".js" :: rest ->
    has_rules
      ~dir
      (match rest with
       | [] -> Subdir_set.all
       | _ -> Subdir_set.empty)
      (fun () ->
        (* XXX the use of the super context is dubious here. We're using it to
           take into account the env stanza. But really, these are internal
           libraries that are being compiled and user settings should be
           ignored. *)
        let* sctx = sctx in
        Jsoo_rules.setup_separate_compilation_rules sctx rest)
  | "_doc" :: rest ->
    let* sctx = sctx in
    Odoc.gen_rules sctx rest ~dir
  | "_doc_new" :: rest ->
    let* sctx = sctx in
    Odoc_new.gen_rules sctx rest ~dir
  | ".topmod" :: comps ->
    has_rules
      ~dir
      (match comps with
       | [] -> Subdir_set.all
       | _ -> Subdir_set.empty)
      (fun () ->
        let* sctx = sctx in
        Top_module.gen_rules sctx ~dir ~comps)
  | ".ppx" :: rest ->
    has_rules
      ~dir
      (match rest with
       | [] -> Subdir_set.all
       | _ -> Subdir_set.empty)
      (fun () ->
        let* sctx = sctx in
        Preprocessing.gen_rules sctx rest)
  | [ ".dune" ] ->
    has_rules
      ~dir
      (Subdir_set.of_set (Filename.Set.of_list [ "ccomp" ]))
      (fun () -> Configurator_rules.gen_rules ctx)
  | _ -> gen_rules_regular_directory sctx ~src_dir ~components ~dir
;;

let with_context ctx ~f =
  Super_context.find ctx
  >>= function
  | None -> Memo.return Gen_rules.unknown_context
  | Some ctx -> f ctx
;;

let analyze_private_context_path components =
  match components with
  | [] -> Memo.return `Root
  | ctx :: components ->
    (match Context_name.of_string_opt ctx with
     | None -> Memo.return `Invalid_context
     | Some ctx ->
       Per_context.valid ctx
       >>| (function
        | true -> `Valid (ctx, components)
        | false -> `Invalid_context))
;;

let private_context ~dir components _ctx =
  analyze_private_context_path components
  >>= function
  | `Invalid_context -> Memo.return Gen_rules.unknown_context
  | `Valid (ctx, components) -> Pkg_rules.setup_rules ctx ~dir ~components
  | `Root ->
    let+ contexts = Per_context.list () in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton
        ~dir
        (Subdir_set.of_list (List.rev_map contexts ~f:Context_name.to_string))
    in
    Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
;;

let gen_rules ctx ~dir components =
  if Context_name.equal ctx Install.Context.install_context.name
  then (
    match components with
    | [] ->
      let+ build_dir_only_sub_dirs =
        let+ context_dirs =
          let+ workspace = Workspace.workspace () in
          Workspace.build_contexts workspace
          |> List.map ~f:(fun (ctx : Build_context.t) -> Context_name.to_string ctx.name)
          |> Subdir_set.of_list
        in
        Gen_rules.Build_only_sub_dirs.singleton ~dir context_dirs
      in
      Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
    | ctx :: _ ->
      let ctx = Context_name.of_string ctx in
      with_context ctx ~f:(fun sctx ->
        let+ subdirs, rules = Install_rules.symlink_rules sctx ~dir in
        let directory_targets = Rules.directory_targets rules in
        Gen_rules.make
          ~build_dir_only_sub_dirs:(Gen_rules.Build_only_sub_dirs.singleton ~dir subdirs)
          ~directory_targets
          (Memo.return rules)))
  else if Context_name.equal ctx Private_context.t.name
  then private_context ~dir components ctx
  else gen_rules ctx (Super_context.find_exn ctx) ~dir components
;;
