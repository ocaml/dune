open Import

(* we need to convince ocamldep that we don't depend on the menhir rules *)
module Menhir = struct end

open Dune_file
open Memo.O

let loc_of_dune_file st_dir =
  Loc.in_file
    (Path.source
       (match Source_tree.Dir.dune_file st_dir with
       | Some d -> Source_tree.Dune_file.path d
       | None -> Path.Source.relative (Source_tree.Dir.path st_dir) "_unknown_"))

type t =
  { kind : kind
  ; dir : Path.Build.t
  ; text_files : String.Set.t
  ; foreign_sources : Foreign_sources.t Memo.Lazy.t
  ; mlds : (Documentation.t * Path.Build.t list) list Memo.Lazy.t
  ; coq : Coq_sources.t Memo.Lazy.t
  ; ml : Ml_sources.t Memo.Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list
  | Group_part

let empty kind ~dir =
  { kind
  ; dir
  ; text_files = String.Set.empty
  ; ml = Memo.Lazy.of_val Ml_sources.empty
  ; mlds = Memo.Lazy.of_val []
  ; foreign_sources = Memo.Lazy.of_val Foreign_sources.empty
  ; coq = Memo.Lazy.of_val Coq_sources.empty
  }

type standalone_or_root =
  { root : t
  ; subdirs : t list
  ; rules : Rules.t
  }

type triage =
  | Standalone_or_root of
      { directory_targets : Loc.t Path.Build.Map.t
      ; contents : standalone_or_root Memo.Lazy.t
      }
  | Group_part of Path.Build.t

let dir t = t.dir

let coq t = Memo.Lazy.force t.coq

let ocaml t = Memo.Lazy.force t.ml

let artifacts t = Memo.Lazy.force t.ml >>= Ml_sources.artifacts

let dirs t =
  match t.kind with
  | Standalone -> [ t ]
  | Group_root subs -> t :: subs
  | Group_part ->
    Code_error.raise "Dir_contents.dirs called on a group part"
      [ ("dir", Path.Build.to_dyn t.dir) ]

let text_files t = t.text_files

let foreign_sources t = Memo.Lazy.force t.foreign_sources

let mlds t (doc : Documentation.t) =
  let+ map = Memo.Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
        Option.some_if (Loc.equal doc.loc doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Code_error.raise "Dir_contents.mlds"
      [ ("doc", Loc.to_dyn_hum doc.loc)
      ; ( "available"
        , Dyn.(list Loc.to_dyn_hum)
            (List.map map ~f:(fun ((d : Documentation.t), _) -> d.loc)) )
      ]

let build_mlds_map stanzas ~dir ~files =
  let mlds =
    Memo.lazy_ (fun () ->
        String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
            match String.lsplit2 fn ~on:'.' with
            | Some (s, "mld") -> String.Map.set acc s fn
            | _ -> acc)
        |> Memo.return)
  in
  Memo.parallel_map stanzas ~f:(function
    | Documentation doc ->
      let+ mlds =
        let+ mlds = Memo.Lazy.force mlds in
        Ordered_set_lang.Unordered_string.eval doc.mld_files ~standard:mlds
          ~key:(fun x -> x)
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s -> s
            | None ->
              User_error.raise ~loc
                [ Pp.textf "%s.mld doesn't exist in %s" s
                    (Path.to_string_maybe_quoted
                       (Path.drop_optional_build_context (Path.build dir)))
                ])
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.Build.relative dir))
    | _ -> Memo.return None)
  >>| List.filter_opt

module rec Load : sig
  val get : Super_context.t -> dir:Path.Build.t -> t Memo.t

  val triage : Super_context.t -> dir:Path.Build.t -> triage Memo.t

  val add_sources_to_expander : Super_context.t -> Expander.t -> Expander.t
end = struct
  let add_sources_to_expander sctx expander =
    let f ~dir = Load.get sctx ~dir >>= artifacts in
    Expander.set_lookup_ml_sources expander ~f

  (* As a side-effect, setup user rules and copy_files rules. *)
  let load_text_files sctx st_dir stanzas ~dir ~src_dir =
    (* Interpret a few stanzas in order to determine the list of files generated
       by the user. *)
    let* expander =
      Super_context.expander sctx ~dir >>| add_sources_to_expander sctx
    in
    let+ generated_files =
      Memo.parallel_map stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          | Coq_stanza.Coqpp.T { modules; _ } ->
            let+ mlg_files = Coq_sources.mlg_files ~sctx ~dir ~modules in
            List.rev_map mlg_files ~f:(fun mlg_file ->
                Path.Build.set_extension mlg_file ~ext:".ml"
                |> Path.Build.basename)
          | Coq_stanza.Extraction.T s ->
            Memo.return (Coq_stanza.Extraction.ml_target_fnames s)
          | Menhir_stanza.T menhir -> Memo.return (Menhir_stanza.targets menhir)
          | Rule rule -> (
            Simple_rules.user_rule sctx rule ~dir ~expander >>| function
            | None -> []
            | Some targets ->
              (* CR-someday amokhov: Do not ignore directory targets. *)
              Path.Build.Set.to_list_map targets.files ~f:Path.Build.basename)
          | Copy_files def ->
            let+ ps =
              Simple_rules.copy_files sctx def ~src_dir ~dir ~expander
            in
            Path.Set.to_list_map ps ~f:Path.basename
          | Generate_sites_module def ->
            let+ res = Generate_sites_module_rules.setup_rules sctx ~dir def in
            [ res ]
          | Library { buildable; _ } | Executables { buildable; _ } ->
            let select_deps_files =
              (* Manually add files generated by the (select ...)
                 dependencies *)
              List.filter_map buildable.libraries ~f:(fun dep ->
                  match (dep : Lib_dep.t) with
                  | Re_export _ | Direct _ -> None
                  | Select s -> Some s.result_fn)
            in
            let ctypes_files =
              (* Also manually add files generated by ctypes rules. *)
              match buildable.ctypes with
              | None -> []
              | Some ctypes -> Ctypes_stanza.generated_ml_and_c_files ctypes
            in
            Memo.return (select_deps_files @ ctypes_files)
          | _ -> Memo.return [])
      >>| fun l -> String.Set.of_list (List.concat l)
    in
    String.Set.union generated_files (Source_tree.Dir.files st_dir)

  type result0_here =
    { t : t
    ; (* [rules] includes rules for subdirectories too *)
      rules : Rules.t
    ; (* The [kind] of the nodes must be Group_part *)
      subdirs : t Path.Build.Map.t
    }

  type result0 =
    | See_above of Path.Build.t
    | Here of
        { directory_targets : Loc.t Path.Build.Map.t
        ; contents : result0_here Memo.Lazy.t
        }

  module Key = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Path.Build.t

    let to_dyn (sctx, path) =
      Dyn.Tuple [ Super_context.to_dyn sctx; Path.Build.to_dyn path ]

    let equal = Tuple.T2.equal Super_context.equal Path.Build.equal

    let hash = Tuple.T2.hash Super_context.hash Path.Build.hash
  end

  let lookup_vlib sctx ~dir =
    let* t = Load.get sctx ~dir in
    Memo.Lazy.force t.ml

  let collect_group ~st_dir ~dir =
    let rec walk st_dir ~dir ~local =
      let* status = Dir_status.DB.get ~dir in
      match status with
      | Is_component_of_a_group_but_not_the_root { stanzas; group_root = _ } ->
        let+ l = walk_children st_dir ~dir ~local in
        Appendable_list.( @ )
          (Appendable_list.singleton (dir, List.rev local, st_dir, stanzas))
          l
      | Generated | Source_only _ | Standalone _ | Group_root _ ->
        Memo.return Appendable_list.empty
    and walk_children st_dir ~dir ~local =
      let+ l =
        Memo.parallel_map
          (Source_tree.Dir.sub_dirs st_dir |> String.Map.to_list)
          ~f:(fun (basename, st_dir) ->
            let* st_dir = Source_tree.Dir.sub_dir_as_t st_dir in
            let dir = Path.Build.relative dir basename in
            let local = basename :: local in
            walk st_dir ~dir ~local)
      in
      Appendable_list.concat l
    in
    let+ l = walk_children st_dir ~dir ~local:[] in
    Appendable_list.to_list l

  let extract_directory_targets ~dir stanzas =
    List.fold_left stanzas ~init:Path.Build.Map.empty ~f:(fun acc stanza ->
        match stanza with
        | Rule { targets = Static { targets = l; _ }; loc = rule_loc; _ } ->
          List.fold_left l ~init:acc ~f:(fun acc (target, kind) ->
              let loc = String_with_vars.loc target in
              match (kind : Targets_spec.Kind.t) with
              | File -> acc
              | Directory -> (
                match String_with_vars.text_only target with
                | None ->
                  User_error.raise ~loc
                    [ Pp.text "Variables are not allowed in directory targets."
                    ]
                | Some target ->
                  let dir_target =
                    Path.Build.relative ~error_loc:loc dir target
                  in
                  if not (Path.Build.is_descendant dir_target ~of_:dir) then
                    (* This will be checked when we interpret the stanza
                       completely, so just ignore this rule for now. *)
                    acc
                  else
                    (* We ignore duplicates here as duplicates are detected and
                       reported by [Load_rules]. *)
                    Path.Build.Map.set acc dir_target rule_loc))
        | _ -> acc)

  let get0_impl (sctx, dir) : result0 Memo.t =
    let ctx = Super_context.context sctx in
    let lib_config = (Super_context.context sctx).lib_config in
    let* status = Dir_status.DB.get ~dir in
    let human_readable_description () =
      Pp.textf "Computing directory contents of %s"
        (Path.to_string_maybe_quoted (Path.build dir))
    in
    match status with
    | Is_component_of_a_group_but_not_the_root { group_root; stanzas = _ } ->
      Memo.return (See_above group_root)
    | Generated | Source_only _ ->
      Memo.return
        (Here
           { directory_targets = Path.Build.Map.empty
           ; contents =
               Memo.lazy_ (fun () ->
                   Memo.return
                     { t = empty Standalone ~dir
                     ; rules = Rules.empty
                     ; subdirs = Path.Build.Map.empty
                     })
           })
    | Standalone (st_dir, d) ->
      let src_dir = d.dir in
      let dune_version = Dune_project.dune_version d.project in
      Memo.return
        (Here
           { directory_targets = extract_directory_targets ~dir d.stanzas
           ; contents =
               Memo.lazy_ ~human_readable_description (fun () ->
                   let include_subdirs = (Loc.none, Include_subdirs.No) in
                   let+ files, rules =
                     Rules.collect (fun () ->
                         load_text_files sctx st_dir d.stanzas ~src_dir ~dir)
                   in
                   let dirs = [ (dir, [], files) ] in
                   let ml =
                     Memo.lazy_ (fun () ->
                         let lookup_vlib = lookup_vlib sctx in
                         let loc = loc_of_dune_file st_dir in
                         let* scope = Scope.DB.find_by_dir dir in
                         Ml_sources.make d ~dir ~scope ~lib_config ~loc
                           ~include_subdirs ~lookup_vlib ~dirs)
                   in
                   { t =
                       { kind = Standalone
                       ; dir
                       ; text_files = files
                       ; ml
                       ; mlds =
                           Memo.lazy_ (fun () ->
                               build_mlds_map d.stanzas ~dir ~files)
                       ; foreign_sources =
                           Memo.lazy_ (fun () ->
                               Foreign_sources.make d.stanzas ~dune_version
                                 ~lib_config:ctx.lib_config ~include_subdirs
                                 ~dirs
                               |> Memo.return)
                       ; coq =
                           Memo.lazy_ (fun () ->
                               Coq_sources.of_dir d.stanzas ~dir
                                 ~include_subdirs ~dirs
                               |> Memo.return)
                       }
                   ; rules
                   ; subdirs = Path.Build.Map.empty
                   })
           })
    | Group_root (st_dir, qualif_mode, d) ->
      let loc = loc_of_dune_file st_dir in
      let include_subdirs =
        let loc, qualif_mode = qualif_mode in
        (loc, Include_subdirs.Include qualif_mode)
      in
      let* subdirs = collect_group ~st_dir ~dir in
      let directory_targets =
        let dirs = (dir, [], st_dir, Some d) :: subdirs in
        List.fold_left dirs ~init:Path.Build.Map.empty
          ~f:(fun acc (dir, _, _, d) ->
            match d with
            | None -> acc
            | Some (d : Dune_file.t) ->
              Path.Build.Map.union acc
                (extract_directory_targets ~dir d.stanzas) ~f:(fun _ _ x ->
                  Some x))
      in
      let contents =
        Memo.lazy_ ~human_readable_description (fun () ->
            let+ (files, (subdirs : (Path.Build.t * _ * _) list)), rules =
              Rules.collect (fun () ->
                  Memo.fork_and_join
                    (fun () ->
                      load_text_files sctx st_dir d.stanzas ~src_dir:d.dir ~dir)
                    (fun () ->
                      Memo.parallel_map subdirs
                        ~f:(fun (dir, local, st_dir, stanzas) ->
                          let+ files =
                            match stanzas with
                            | None -> Memo.return (Source_tree.Dir.files st_dir)
                            | Some d ->
                              load_text_files sctx st_dir d.stanzas
                                ~src_dir:d.dir ~dir
                          in
                          (dir, local, files))))
            in
            let dirs = (dir, [], files) :: subdirs in
            let ml =
              Memo.lazy_ (fun () ->
                  let lookup_vlib = lookup_vlib sctx in
                  let* scope = Scope.DB.find_by_dir dir in
                  Ml_sources.make d ~dir ~scope ~lib_config ~loc ~lookup_vlib
                    ~include_subdirs ~dirs)
            in
            let dune_version = Dune_project.dune_version d.project in
            let foreign_sources =
              Memo.lazy_ (fun () ->
                  Foreign_sources.make d.stanzas ~dune_version ~include_subdirs
                    ~lib_config:ctx.lib_config ~dirs
                  |> Memo.return)
            in
            let coq =
              Memo.lazy_ (fun () ->
                  Coq_sources.of_dir d.stanzas ~dir ~dirs ~include_subdirs
                  |> Memo.return)
            in
            let subdirs =
              List.map subdirs ~f:(fun (dir, _local, files) ->
                  { kind = Group_part
                  ; dir
                  ; text_files = files
                  ; ml
                  ; foreign_sources
                  ; mlds =
                      Memo.lazy_ (fun () ->
                          build_mlds_map d.stanzas ~dir ~files)
                  ; coq
                  })
            in
            let t =
              { kind = Group_root subdirs
              ; dir
              ; text_files = files
              ; ml
              ; foreign_sources
              ; mlds =
                  Memo.lazy_ (fun () -> build_mlds_map d.stanzas ~dir ~files)
              ; coq
              }
            in
            { t
            ; rules
            ; subdirs =
                Path.Build.Map.of_list_map_exn subdirs ~f:(fun x -> (x.dir, x))
            })
      in
      Memo.return (Here { directory_targets; contents })

  let memo0 =
    Memo.create "dir-contents-get0" get0_impl
      ~input:(module Key)
      ~human_readable_description:(fun (_, dir) ->
        Pp.textf "Computing directory contents of %s"
          (Path.to_string_maybe_quoted (Path.build dir)))

  let get sctx ~dir =
    Memo.exec memo0 (sctx, dir) >>= function
    | Here { directory_targets = _; contents } ->
      let+ { t; rules = _; subdirs = _ } = Memo.Lazy.force contents in
      t
    | See_above group_root -> (
      Memo.exec memo0 (sctx, group_root) >>= function
      | See_above _ -> assert false
      | Here { directory_targets = _; contents } ->
        let+ { t; rules = _; subdirs = _ } = Memo.Lazy.force contents in
        t)

  let triage sctx ~dir =
    Memo.exec memo0 (sctx, dir) >>| function
    | See_above group_root -> Group_part group_root
    | Here { directory_targets; contents } ->
      Standalone_or_root
        { directory_targets
        ; contents =
            Memo.Lazy.map contents ~f:(fun { t; rules; subdirs } ->
                { root = t; subdirs = Path.Build.Map.values subdirs; rules })
        }
end

include Load

let modules_of_lib sctx lib =
  let dir = Lib_info.src_dir (Lib.info lib) in
  match Path.as_in_build_dir dir with
  | None -> Memo.return None
  | Some dir ->
    let* t = get sctx ~dir in
    let+ ml_sources = ocaml t in
    let name = Lib.name lib in
    Some (Ml_sources.modules ml_sources ~for_:(Library name))
