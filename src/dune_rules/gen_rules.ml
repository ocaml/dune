open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let install_stanza_rules ~ctx_dir ~expander (install_conf : Dune_file.Install_conf.t) =
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
    match stanza with
    | Toplevel toplevel ->
      let+ () = toplevel_setup ~sctx ~dir ~toplevel in
      empty_none
    | Library lib ->
      (* XXX why are we setting up private doc rules for disabled libraries? *)
      let* () = Odoc.setup_private_library_doc_alias sctx ~scope ~dir:ctx_dir lib
      and+ enabled_if =
        Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
      in
      if_available_buildable
        ~loc:lib.buildable.loc
        (fun () -> Lib_rules.rules lib ~sctx ~dir ~scope ~dir_contents ~expander)
        enabled_if
    | Foreign_library lib ->
      let+ () = Lib_rules.foreign_rules lib ~sctx ~dir ~dir_contents ~expander in
      empty_none
    | Executables exes ->
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
    | Alias alias ->
      let+ () = Simple_rules.alias sctx alias ~dir ~expander in
      empty_none
    | Tests tests ->
      Expander.eval_blang expander tests.build_if
      >>= if_available_buildable ~loc:tests.exes.buildable.loc (fun () ->
        Test_rules.rules tests ~sctx ~dir ~scope ~expander ~dir_contents)
    | Copy_files { files = glob; _ } ->
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
    | Install i ->
      let+ () = install_stanza_rules ~ctx_dir ~expander i in
      empty_none
    | Plugin p ->
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
      match (stanza : Stanza.t) with
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

let collect_directory_targets ~init ~dir =
  Only_packages.stanzas_in_dir dir
  >>= function
  | None -> Memo.return init
  | Some d ->
    Memo.List.fold_left d.stanzas ~init ~f:(fun acc stanza ->
      match stanza with
      | Coq_stanza.Theory.T m ->
        Coq_rules.coqdoc_directory_targets ~dir m
        >>| Path.Build.Map.union acc ~f:(fun path loc1 loc2 ->
          User_error.raise
            ~loc:loc1
            [ Pp.textf
                "The following both define the same directory target: %s"
                (Path.Build.to_string path)
            ; Pp.enumerate ~f:Loc.pp_file_colon_line [ loc1; loc2 ]
            ])
      | _ -> Memo.return acc)
;;

let gen_rules sctx dir_contents cctxs ~source_dir ~dir
  : (Loc.t * Compilation_context.t) list Memo.t
  =
  let* expander =
    let+ expander = Super_context.expander sctx ~dir in
    Dir_contents.add_sources_to_expander sctx expander
  and+ () = Format_rules.setup_alias sctx ~dir
  and+ tests = Source_tree.Dir.cram_tests source_dir
  and+ stanzas =
    Only_packages.stanzas_in_dir dir
    >>= function
    | Some d -> Memo.return (Some d)
    | None ->
      let* scope = Scope.DB.find_by_dir dir in
      let project = Scope.project scope in
      let+ () = define_all_alias ~dir ~js_targets:[] ~project in
      None
  in
  let+ () = Cram_rules.rules ~sctx ~expander ~dir tests
  and+ contexts =
    match stanzas with
    | None -> Memo.return []
    | Some d -> gen_rules_for_stanzas sctx dir_contents cctxs expander d ~dir
  in
  contexts
;;

(* To be called once per project, when we are generating the rules for the root
   directory of the project *)
let gen_project_rules sctx project : unit Memo.t =
  let+ () = Install_rules.gen_project_rules sctx project
  and+ () = Odoc.gen_project_rules sctx project
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
           Warning.missing_project_name
           (Warning_emit.Context.project project)
           (fun () ->
              let+ () = Memo.return () in
              let loc = Loc.in_file (Path.source (Dune_project.file project)) in
              User_message.make
                ~loc
                [ Pp.text
                    "Project name is not specified. Add a (name <project-name>) field to \
                     your dune-project file to make sure that $ dune subst works in \
                     release or pinned builds"
                ]))
  in
  ()
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

  let of_src_dir src_dir = Filename.Map.find map (Path.Source.basename src_dir)

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
      Super_context.local_binaries sctx ~dir:(Path.Build.parent_exn dir)
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

module For_melange = struct
  (* The emit stanza of melange outputs in a single output directory (and its
     descendants). We attach all .js generating rules to this root directory.

     Since we allow user defined rules in this output directory, we need to know
     when we're under the emit directory so that we load both the user defined
     rules and the rules originating from the emit stanza. *)
  type t =
    { (* the directory in which the emit stanza is defined. *)
      stanza_dir : Path.Build.t
    ; (* the emit stanza itself. *)
      stanza : Melange_stanzas.Emit.t
    }

  let emit_rules sctx { stanza_dir; stanza } =
    Rules.collect_unit (fun () ->
      let* dir_contents = Dir_contents.get sctx ~dir:stanza_dir in
      let* scope = Scope.DB.find_by_dir stanza_dir in
      Melange_rules.setup_emit_js_rules ~dir_contents ~dir:stanza_dir ~scope ~sctx stanza)
  ;;

  (* Detect if [dir] is under the target directory of a melange.emit stanza. *)
  let rec under_melange_emit_target ~dir =
    match Path.Build.parent dir with
    | None -> Memo.return None
    | Some parent ->
      Only_packages.stanzas_in_dir parent
      >>= (function
       | None -> under_melange_emit_target ~dir:parent
       | Some stanzas ->
         (match
            List.find_map stanzas.stanzas ~f:(function
              | Melange_stanzas.Emit.T mel ->
                let target_dir = Melange_stanzas.Emit.target_dir ~dir:parent mel in
                Option.some_if (Path.Build.equal target_dir dir) mel
              | _ -> None)
          with
          | None -> under_melange_emit_target ~dir:parent
          | Some stanza -> Memo.return @@ Some { stanza_dir = parent; stanza }))
  ;;

  let gen_emit_rules sctx ~dir ({ stanza_dir; stanza } as for_melange) =
    match
      Path.Build.equal dir (Melange_stanzas.Emit.target_dir ~dir:stanza_dir stanza)
    with
    | false -> Memo.return None
    | true ->
      under_melange_emit_target ~dir:stanza_dir
      >>| (function
       | None -> Some (emit_rules sctx for_melange)
       | Some { stanza_dir = parent_melange_emit_dir; stanza = parent_stanza } ->
         let main_message = Pp.text "melange.emit stanzas cannot be nested" in
         let annots =
           let main = User_message.make ~loc:stanza.loc [ main_message ] in
           let related =
             [ User_message.make
                 ~loc:parent_stanza.loc
                 [ Pp.text "under this melange stanza" ]
             ]
           in
           User_message.Annots.singleton
             Compound_user_error.annot
             [ Compound_user_error.make ~main ~related ]
         in
         User_error.raise
           ~loc:stanza.loc
           ~annots
           [ main_message
           ; Pp.enumerate ~f:Loc.pp_file_colon_line [ parent_stanza.loc; stanza.loc ]
           ]
           ~hints:
             (let emit_dir = Path.Build.drop_build_context_exn stanza_dir in
              let parent_melange_emit_dir =
                Path.Build.drop_build_context_exn parent_melange_emit_dir
              in
              [ Pp.textf
                  "Move the melange.emit stanza from %s to at least the level of %s"
                  (Path.Source.to_string emit_dir)
                  (Path.Source.to_string parent_melange_emit_dir)
              ]))
  ;;
end

let gen_melange_emit_rules_or_empty_redirect sctx ~dir under_melange_emit =
  let rules =
    match under_melange_emit with
    | None -> Memo.return Rules.empty
    | Some for_melange ->
      For_melange.gen_emit_rules sctx ~dir for_melange
      >>= (function
       | Some r -> r
       | None -> Memo.return Rules.empty)
  in
  Gen_rules.redirect_to_parent
    (Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rules)
;;

let gen_rules_standalone_or_root
  sctx
  standalone_or_root
  ~dir
  ~source_dir
  ~under_melange_emit_target
  =
  let rules =
    let* () = Memo.Lazy.force Configurator_rules.force_files in
    let* rules' =
      Rules.collect_unit (fun () ->
        let* () =
          let project = Source_tree.Dir.project source_dir in
          if Path.Source.equal
               (Source_tree.Dir.path source_dir)
               (Dune_project.root project)
          then gen_project_rules sctx project
          else Memo.return ()
        in
        let* dir_contents = Dir_contents.Standalone_or_root.root standalone_or_root in
        let* cctxs = gen_rules sctx dir_contents [] ~source_dir ~dir in
        Dir_contents.Standalone_or_root.subdirs standalone_or_root
        >>= Memo.parallel_iter ~f:(fun dc ->
          let+ (_ : (Loc.t * Compilation_context.t) list) =
            gen_rules sctx dir_contents cctxs ~source_dir ~dir:(Dir_contents.dir dc)
          in
          ()))
    in
    let+ rules = Dir_contents.Standalone_or_root.rules standalone_or_root in
    Rules.union rules rules'
  in
  let* rules =
    let+ directory_targets =
      let init = Dir_contents.Standalone_or_root.directory_targets standalone_or_root in
      collect_directory_targets ~dir ~init
    in
    Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rules ~directory_targets
  in
  match under_melange_emit_target with
  | None -> Memo.return @@ Gen_rules.rules_here rules
  | Some for_melange ->
    let+ melange_rules = For_melange.gen_emit_rules sctx ~dir for_melange in
    Gen_rules.redirect_to_parent
    @@
      (match melange_rules with
      | None -> rules
      | Some emit ->
        Gen_rules.Rules.combine_exn
          rules
          (Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty emit))
;;

let gen_rules_build_dir sctx ~dir ~nearest_src_dir ~src_dir ~under_melange_emit_target =
  (* There is always a source dir at the root, so we can't be at the root if
     we are in this branch *)
  match nearest_src_dir with
  | None ->
    Memo.return
    @@ gen_melange_emit_rules_or_empty_redirect sctx ~dir under_melange_emit_target
  | Some _ ->
    (match Automatic_subdir.of_src_dir src_dir with
     | Some kind ->
       has_rules ~dir Subdir_set.empty (fun () ->
         Automatic_subdir.gen_rules ~sctx ~dir kind)
     | None ->
       Memo.return
       @@ gen_melange_emit_rules_or_empty_redirect sctx ~dir under_melange_emit_target)
;;

let gen_rules_regular_directory sctx ~components ~dir =
  let src_dir = Path.Build.drop_build_context_exn dir in
  let* under_melange_emit_target = For_melange.under_melange_emit_target ~dir in
  let* st_dir = Source_tree.find_dir src_dir in
  let* nearest_src_dir =
    match st_dir with
    | Some dir -> Memo.return (Some dir)
    | None -> Source_tree.find_dir (Path.Source.parent_exn src_dir)
  in
  let* rules =
    match st_dir with
    | None ->
      gen_rules_build_dir sctx ~nearest_src_dir ~dir ~src_dir ~under_melange_emit_target
    | Some source_dir ->
      (* This interprets [rule] and [copy_files] stanzas. *)
      Dir_contents.triage sctx ~dir
      >>= (function
       | Group_part _ ->
         Memo.return
         @@ gen_melange_emit_rules_or_empty_redirect sctx ~dir under_melange_emit_target
       | Standalone_or_root standalone_or_root ->
         gen_rules_standalone_or_root
           sctx
           standalone_or_root
           ~dir
           ~source_dir
           ~under_melange_emit_target)
  in
  let* rules =
    Gen_rules.map_rules rules ~f:(fun (rules : Gen_rules.Rules.t) ->
      let+ build_dir_only_sub_dirs =
        let+ allowed_subdirs =
          (let automatic = Automatic_subdir.subdirs components in
           let toplevel =
             match components with
             | _ :: _ -> Filename.Set.empty
             | [] ->
               (* XXX sync this list with the pattern matches above. It's quite ugly
                  we need this, we should rewrite this code to avoid this. *)
               Filename.Set.of_list [ ".js"; "_doc"; ".ppx"; ".dune"; ".topmod" ]
           in
           let+ melange =
             match under_melange_emit_target with
             | Some _ -> Memo.return Filename.Set.empty
             | None ->
               (* this should probably be handled by [Dir_status] *)
               Only_packages.stanzas_in_dir dir
               >>| (function
                | None -> Filename.Set.empty
                | Some dune_file ->
                  List.filter_map dune_file.stanzas ~f:(function
                    | Melange_stanzas.Emit.T mel -> Some mel.target
                    | _ -> None)
                  |> Filename.Set.of_list)
           in
           Filename.Set.union_all [ automatic; toplevel; melange ])
          >>| Subdir_set.of_set
          >>| Gen_rules.Build_only_sub_dirs.singleton ~dir
        in
        Gen_rules.Build_only_sub_dirs.union rules.build_dir_only_sub_dirs allowed_subdirs
      in
      { rules with build_dir_only_sub_dirs })
  in
  match Opam_create.gen_rules sctx ~dir ~nearest_src_dir ~src_dir with
  | None -> Memo.return rules
  | Some opam_rules ->
    Gen_rules.map_rules rules ~f:(fun rules ->
      Memo.return (Gen_rules.Rules.combine_exn opam_rules rules))
;;

(* Once [gen_rules] has decided what to do with the directory, it should end
   with [has_rules] or [redirect_to_parent] *)
let gen_rules ~sctx ~dir components : Gen_rules.result Memo.t =
  match components with
  | [ ".dune"; "ccomp" ] ->
    has_rules ~dir Subdir_set.empty (fun () ->
      (* Add rules for C compiler detection *)
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
        Jsoo_rules.setup_separate_compilation_rules sctx rest)
  | "_doc" :: rest -> Odoc.gen_rules sctx rest ~dir
  | ".topmod" :: comps ->
    has_rules
      ~dir
      (match comps with
       | [] -> Subdir_set.all
       | _ -> Subdir_set.empty)
      (fun () -> Top_module.gen_rules sctx ~dir ~comps)
  | ".ppx" :: rest ->
    has_rules
      ~dir
      (match rest with
       | [] -> Subdir_set.all
       | _ -> Subdir_set.empty)
      (fun () -> Preprocessing.gen_rules sctx rest)
  | _ -> gen_rules_regular_directory sctx ~components ~dir
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
  else (
    match components with
    | [ ".dune" ] ->
      has_rules
        ~dir
        (Subdir_set.of_set (Filename.Set.of_list [ "ccomp" ]))
        (fun () -> Context.DB.get ctx >>= Configurator_rules.gen_rules)
    | _ -> with_context ctx ~f:(fun sctx -> gen_rules ~sctx ~dir components))
;;
