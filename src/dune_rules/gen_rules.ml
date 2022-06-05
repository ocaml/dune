open Import
module Menhir_rules = Menhir
module Toplevel_rules = Toplevel.Stanza
open Dune_file
open Memo.O

module For_stanza : sig
  type ('merlin, 'cctx, 'js, 'source_dirs) t =
    { merlin : 'merlin
    ; cctx : 'cctx
    ; js : 'js
    ; source_dirs : 'source_dirs
    }

  val of_stanzas :
       Stanza.t list
    -> cctxs:(Loc.t * Compilation_context.t) list
    -> sctx:Super_context.t
    -> src_dir:Path.Source.t
    -> ctx_dir:Path.Build.t
    -> scope:Scope.t
    -> dir_contents:Dir_contents.t
    -> expander:Expander.t
    -> files_to_install:(Install_conf.t -> unit Memo.t)
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

  let cons acc x =
    { merlin = cons_maybe x.merlin acc.merlin
    ; cctx = cons_maybe x.cctx acc.cctx
    ; source_dirs = cons_maybe x.source_dirs acc.source_dirs
    ; js =
        (match x.js with
        | None -> acc.js
        | Some js -> List.rev_append acc.js js)
    }

  let rev t =
    { t with
      merlin = List.rev t.merlin
    ; cctx = List.rev t.cctx
    ; source_dirs = List.rev t.source_dirs
    }

  let of_stanza stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander
      ~files_to_install =
    let dir = ctx_dir in
    match stanza with
    | Toplevel toplevel ->
      let+ () = Toplevel_rules.setup ~sctx ~dir ~toplevel in
      empty_none
    | Library lib ->
      let* () =
        Odoc.setup_private_library_doc_alias sctx ~scope ~dir:ctx_dir lib
      in
      let* available =
        Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
      in
      if available then
        let+ cctx, merlin =
          Lib_rules.rules lib ~sctx ~dir ~scope ~dir_contents ~expander
        in
        { merlin = Some merlin
        ; cctx = Some (lib.buildable.loc, cctx)
        ; js = None
        ; source_dirs = None
        }
      else Memo.return empty_none
    | Foreign_library lib ->
      let+ () =
        Lib_rules.foreign_rules lib ~sctx ~dir ~dir_contents ~expander
      in
      empty_none
    | Executables exes -> (
      Expander.eval_blang expander exes.enabled_if >>= function
      | false -> Memo.return empty_none
      | true ->
        let* () = Memo.Option.iter exes.install_conf ~f:files_to_install in
        let+ cctx, merlin =
          Exe_rules.rules exes ~sctx ~dir ~scope ~expander ~dir_contents
        in
        { merlin = Some merlin
        ; cctx = Some (exes.buildable.loc, cctx)
        ; js =
            Some
              (List.concat_map exes.names ~f:(fun (_, exe) ->
                   List.map
                     [ exe ^ ".bc.js"; exe ^ ".bc.runtime.js" ]
                     ~f:(Path.Build.relative dir)))
        ; source_dirs = None
        })
    | Alias alias ->
      let+ () = Simple_rules.alias sctx alias ~dir ~expander in
      empty_none
    | Tests tests ->
      let+ cctx, merlin =
        Test_rules.rules tests ~sctx ~dir ~scope ~expander ~dir_contents
      in
      { merlin = Some merlin
      ; cctx = Some (tests.exes.buildable.loc, cctx)
      ; js = None
      ; source_dirs = None
      }
    | Copy_files { files = glob; _ } ->
      let* source_dirs =
        let loc = String_with_vars.loc glob in
        let+ src_glob = Expander.No_deps.expand_str expander glob in
        if Filename.is_relative src_glob then
          match Path.relative (Path.source src_dir) src_glob ~error_loc:loc with
          | In_source_tree s -> Some (Path.Source.parent_exn s)
          | In_build_dir _ | External _ -> None
        else None
      in
      Memo.return { merlin = None; cctx = None; js = None; source_dirs }
    | Install i ->
      let+ () = files_to_install i in
      empty_none
    | Plugin p ->
      let+ () = Plugin_rules.setup_rules ~sctx ~dir p in
      empty_none
    | Cinaps.T cinaps ->
      let+ () = Cinaps.gen_rules sctx cinaps ~dir ~scope in
      empty_none
    | Mdx.T mdx -> (
      Expander.eval_blang expander (Mdx.enabled_if mdx) >>= function
      | false -> Memo.return empty_none
      | true ->
        let+ () = Mdx.gen_rules ~sctx ~dir ~scope ~expander mdx in
        empty_none)
    | _ -> Memo.return empty_none

  let of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents
      ~expander ~files_to_install =
    let of_stanza =
      of_stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander
        ~files_to_install
    in
    let+ l = Memo.parallel_map stanzas ~f:of_stanza in
    List.fold_left l ~init:{ empty_list with cctx = cctxs } ~f:(fun acc x ->
        cons acc x)
    |> rev
end

(* This is used to determine the list of source directories to give to Merlin.
   This serves the same purpose as [Merlin.lib_src_dirs] and has a similar
   implementation, but this definition is used for the current library, while
   [Merlin.lib_src_dirs] is used for the dependencies. It would be nice to unify
   them at some point. *)
let lib_src_dirs ~dir_contents =
  Dir_contents.dirs dir_contents
  |> List.map ~f:(fun dc ->
         Path.Build.drop_build_context_exn (Dir_contents.dir dc))

(* Stanza *)

let define_all_alias ~dir ~project ~js_targets =
  let deps =
    let pred =
      let id =
        lazy
          (let open Dyn in
          variant "exclude"
            (List.map ~f:(fun p -> Path.Build.to_dyn p) js_targets))
      in
      List.iter js_targets ~f:(fun js_target ->
          assert (Path.Build.equal (Path.Build.parent_exn js_target) dir));
      let f =
        if Dune_project.explicit_js_mode project then fun _ -> true
        else fun basename ->
          not
            (List.exists js_targets ~f:(fun js_target ->
                 String.equal (Path.Build.basename js_target) basename))
      in
      Predicate_with_id.create ~id ~f
    in
    let only_generated_files = Dune_project.dune_version project >= (3, 0) in
    File_selector.create ~dir:(Path.build dir) ~only_generated_files pred
    |> Action_builder.paths_matching_unit ~loc:Loc.none
  in
  Rules.Produce.Alias.add_deps (Alias.all ~dir) deps

let gen_rules sctx dir_contents cctxs expander
    { Dune_file.dir = src_dir; stanzas; project } ~dir:ctx_dir =
  let files_to_install
      { Install_conf.section = _; files; package = _; enabled_if = _ } =
    Memo.List.map files ~f:(fun fb ->
        File_binding.Unexpanded.expand_src ~dir:ctx_dir fb
          ~f:(Expander.No_deps.expand_str expander)
        >>| Path.build)
    >>= fun files ->
    Rules.Produce.Alias.add_deps (Alias.all ~dir:ctx_dir)
      (Action_builder.paths files)
  in
  let* { For_stanza.merlin = merlins
       ; cctx = cctxs
       ; js = js_targets
       ; source_dirs
       } =
    let* scope = Scope.DB.find_by_dir ctx_dir in
    For_stanza.of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope
      ~dir_contents ~expander ~files_to_install
  in
  let* () =
    Memo.sequential_iter merlins ~f:(fun merlin ->
        let more_src_dirs =
          lib_src_dirs ~dir_contents |> List.rev_append (src_dir :: source_dirs)
        in
        Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~expander merlin)
  in
  let* () =
    Memo.parallel_iter stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Menhir.T m -> (
          Expander.eval_blang expander m.enabled_if >>= function
          | false -> Memo.return ()
          | true -> (
            let* ml_sources = Dir_contents.ocaml dir_contents in
            match
              List.find_map (Menhir_rules.module_names m) ~f:(fun name ->
                  Option.bind (Ml_sources.lookup_module ml_sources name)
                    ~f:(fun buildable ->
                      List.find_map cctxs ~f:(fun (loc, cctx) ->
                          Option.some_if (Loc.equal loc buildable.loc) cctx)))
            with
            | None ->
              (* This happens often when passing a [-p ...] option that hides a
                 library *)
              let file_targets =
                List.map
                  (Dune_file.Menhir.targets m)
                  ~f:(Path.Build.relative ctx_dir)
              in
              Super_context.add_rule sctx ~dir:ctx_dir
                (Action_builder.fail
                   { fail =
                       (fun () ->
                         User_error.raise ~loc:m.loc
                           [ Pp.text
                               "I can't determine what library/executable the \
                                files produced by this stanza are part of."
                           ])
                   }
                |> Action_builder.with_file_targets ~file_targets)
            | Some cctx -> Menhir_rules.gen_rules cctx m ~dir:ctx_dir))
        | Coq_stanza.Theory.T m -> (
          Expander.eval_blang expander m.enabled_if >>= function
          | false -> Memo.return ()
          | true -> Coq_rules.setup_rules ~sctx ~dir:ctx_dir ~dir_contents m)
        | Coq_stanza.Extraction.T m ->
          Coq_rules.setup_extraction_rules ~sctx ~dir:ctx_dir ~dir_contents m
        | Coq_stanza.Coqpp.T m ->
          Coq_rules.setup_coqpp_rules ~sctx ~dir:ctx_dir m
        | _ -> Memo.return ())
  in
  let+ () = define_all_alias ~dir:ctx_dir ~project ~js_targets in
  cctxs

let collect_directory_targets ~init ~dir =
  Only_packages.stanzas_in_dir dir >>| function
  | None -> init
  | Some d ->
    List.fold_left d.stanzas ~init ~f:(fun acc stanza ->
        match stanza with
        | Coq_stanza.Theory.T m ->
          Coq_rules.coqdoc_directory_targets ~dir m
          |> Path.Build.Map.union acc ~f:(fun path loc1 loc2 ->
                 User_error.raise
                   [ Pp.textf
                       "the following both define the directory target: %s"
                       (Path.Build.to_string path)
                   ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
                   ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
                   ])
        | _ -> acc)

let gen_rules sctx dir_contents cctxs ~source_dir ~dir :
    (Loc.t * Compilation_context.t) list Memo.t =
  let* expander =
    let+ expander = Super_context.expander sctx ~dir in
    Dir_contents.add_sources_to_expander sctx expander
  and* tests = Source_tree.Dir.cram_tests source_dir in
  let* () = Cram_rules.rules ~sctx ~expander ~dir tests in
  let* () = Format_rules.setup_alias sctx ~dir in
  Only_packages.stanzas_in_dir dir >>= function
  | Some d -> gen_rules sctx dir_contents cctxs expander d ~dir
  | None ->
    let* scope = Scope.DB.find_by_dir dir in
    let project = Scope.project scope in
    let+ () = define_all_alias ~dir ~js_targets:[] ~project in
    []

(* To be called once per project, when we are generating the rules for the root
   diretory of the project *)
let gen_project_rules sctx project =
  let+ () = Opam_create.add_rules sctx project
  and+ () = Install_rules.gen_project_rules sctx project
  and+ () = Odoc.gen_project_rules sctx project in
  ()

(* Sub-dirs that are automatically generated in all directories. Or rather, all
   the ones that have a corresponding source directory. *)
type automatic_sub_dir =
  | Utop
  | Formatted
  | Bin

let bin_dir_basename = ".bin"

let automatic_sub_dirs_map =
  String.Map.of_list_exn
    [ (Utop.utop_dir_basename, Utop)
    ; (Format_rules.formatted_dir_basename, Formatted)
    ; (bin_dir_basename, Bin)
    ]

let gen_rules_for_automatic_sub_dir ~sctx ~dir kind =
  match kind with
  | Utop -> Utop.setup sctx ~dir:(Path.Build.parent_exn dir)
  | Formatted -> Format_rules.gen_rules sctx ~output_dir:dir
  | Bin ->
    let* local_binaries =
      Super_context.local_binaries sctx ~dir:(Path.Build.parent_exn dir)
    in
    Memo.sequential_iter local_binaries ~f:(fun t ->
        let loc = File_binding.Expanded.src_loc t in
        let src = Path.build (File_binding.Expanded.src t) in
        let dst = File_binding.Expanded.dst_path t ~dir in
        Super_context.add_rule sctx ~loc ~dir (Action_builder.symlink ~src ~dst))

let has_rules subdirs f =
  let rules = Rules.collect_unit f in
  Memo.return
    (Build_config.Rules
       { build_dir_only_sub_dirs = subdirs
       ; directory_targets = Path.Build.Map.empty
       ; rules
       })

let redirect_to_parent = Memo.return Build_config.Redirect_to_parent

(* Once [gen_rules] has decided what to do with the directory, it should end
   with [has_rules] or [redirect_to_parent] *)
let gen_rules ~sctx ~dir components : Build_config.gen_rules_result Memo.t =
  let module S = Subdir_set in
  match components with
  | [ ".dune"; "ccomp" ] ->
    has_rules S.empty (fun () ->
        (* Add rules for C compiler detection *)
        Cxx_rules.rules ~sctx ~dir)
  | [ ".dune" ] ->
    has_rules
      (S.These (String.Set.of_list [ "ccomp" ]))
      (fun () -> Context.gen_configurator_rules (Super_context.context sctx))
  | ".js" :: rest ->
    has_rules
      (match rest with
      | [] -> S.All
      | _ -> S.empty)
      (fun () -> Jsoo_rules.setup_separate_compilation_rules sctx rest)
  | "_doc" :: rest -> Odoc.gen_rules sctx rest ~dir
  | ".ppx" :: rest ->
    has_rules
      (match rest with
      | [] -> S.All
      | _ -> S.empty)
      (fun () -> Preprocessing.gen_rules sctx rest)
  | _ -> (
    let src_dir = Path.Build.drop_build_context_exn dir in
    Source_tree.find_dir src_dir >>= function
    | None -> (
      (* There is always a source dir at the root, so we can't be at the root if
         we are in this branch *)
      let parent = Path.Source.parent_exn src_dir in
      Source_tree.find_dir parent >>= function
      | None -> redirect_to_parent
      | Some _ -> (
        match
          String.Map.find automatic_sub_dirs_map (Path.Source.basename src_dir)
        with
        | None -> redirect_to_parent
        | Some kind ->
          has_rules Subdir_set.empty (fun () ->
              gen_rules_for_automatic_sub_dir ~sctx ~dir kind)))
    | Some source_dir -> (
      (* This interprets "rule" and "copy_files" stanzas. *)
      Dir_contents.triage sctx ~dir
      >>= function
      | Group_part _ -> redirect_to_parent
      | Standalone_or_root { directory_targets; contents } ->
        let rules =
          let* () = Memo.Lazy.force Context.force_configurator_files in
          let* { Dir_contents.root = dir_contents; subdirs; rules } =
            Memo.Lazy.force contents
          in
          let* rules' =
            Rules.collect_unit (fun () ->
                let* () =
                  let project = Source_tree.Dir.project source_dir in
                  if
                    Path.Build.equal
                      (Path.Build.append_source
                         (Super_context.context sctx).build_dir
                         (Dune_project.root project))
                      dir
                  then gen_project_rules sctx project
                  else Memo.return ()
                in
                let* cctxs = gen_rules sctx dir_contents [] ~source_dir ~dir in
                Memo.parallel_iter subdirs ~f:(fun dc ->
                    gen_rules sctx dir_contents cctxs ~source_dir
                      ~dir:(Dir_contents.dir dc)
                    >>| ignore))
          in
          Memo.return (Rules.union rules rules')
        in
        let subdirs = String.Set.of_keys automatic_sub_dirs_map in
        let subdirs =
          match components with
          | [] ->
            String.Set.union subdirs
              (String.Set.of_list [ ".js"; "_doc"; ".ppx"; ".dune" ])
          | _ -> subdirs
        in
        let+ directory_targets =
          collect_directory_targets ~dir ~init:directory_targets
        in
        Build_config.Rules
          { build_dir_only_sub_dirs = S.These subdirs
          ; directory_targets
          ; rules
          }))

let with_context ctx ~f =
  Super_context.find ctx >>= function
  | None -> Memo.return Build_config.Unknown_context_or_install
  | Some ctx -> f ctx

let gen_rules ctx_or_install ~dir components =
  match (ctx_or_install : Build_config.Context_or_install.t) with
  | Install ctx ->
    with_context ctx ~f:(fun sctx ->
        let+ subdirs, rules = Install_rules.symlink_rules sctx ~dir in
        Build_config.Rules
          { build_dir_only_sub_dirs = subdirs
          ; directory_targets = Path.Build.Map.empty
          ; rules = Memo.return rules
          })
  | Context ctx ->
    with_context ctx ~f:(fun sctx -> gen_rules ~sctx ~dir components)
