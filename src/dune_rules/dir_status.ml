open Import
open Memo.O

module Is_component_of_a_group_but_not_the_root = struct
  type t =
    { group_root : Path.Build.t
    ; stanzas : Dune_file.t option
    }
end

module Group_component = struct
  type t =
    { dir : Path.Build.t
    ; path_to_group_root : Filename.t list
    ; source_dir : Source_tree.Dir.t
    ; stanzas : Stanza.t list
    }
end

module Group_root = struct
  type t =
    { source_dir : Source_tree.Dir.t
    ; qualification : Loc.t * Include_subdirs.qualification
    ; dune_file : Dune_file.t
    ; components : Group_component.t list Memo.t
    }
end

module T = struct
  type t =
    | Lock_dir
    | Generated
    | Source_only of Source_tree.Dir.t
    | (* Directory not part of a multi-directory group *)
      Standalone of Source_tree.Dir.t * Dune_file.t
    | (* Directory with [(include_subdirs x)] where [x] is not [no] *)
      Group_root of Group_root.t
    | (* Sub-directory of a [Group_root _] *)
      Is_component_of_a_group_but_not_the_root of
        Is_component_of_a_group_but_not_the_root.t
end

include T

type enclosing_group =
  | No_group
  | Group_root of Path.Build.t

let current_group dir = function
  | Lock_dir | Generated | Source_only _ | Standalone _ -> No_group
  | Group_root _ -> Group_root dir
  | Is_component_of_a_group_but_not_the_root { group_root; _ } -> Group_root group_root
;;

let get_include_subdirs include_stanzas =
  match include_stanzas with
  | [] -> None
  | [ (loc, x) ] -> Some (loc, x)
  | _ :: (loc, _) :: _ ->
    User_error.raise
      ~loc
      [ Pp.text "The 'include_subdirs' stanza cannot appear more than once" ]
;;

let find_module_stanza stanzas =
  List.find_map stanzas ~f:(fun stanza ->
    match Stanza.repr stanza with
    | Melange_stanzas.Emit.T { loc; _ }
    | Library.T { buildable = { loc; _ }; _ }
    | Executables.T { buildable = { loc; _ }; _ }
    | Tests.T { exes = { buildable = { loc; _ }; _ }; _ } -> Some loc
    | _ -> None)
;;

let error_no_module_consumer ~loc (qualification : Include_subdirs.qualification) =
  User_error.raise
    ~loc
    ~hints:[ Pp.text "add (include_subdirs no) to this file." ]
    [ Pp.textf
        "This stanza is not allowed in a sub-directory of directory with \
         (include_subdirs %s)."
        (match qualification with
         | Unqualified -> "unqualified"
         | Qualified -> "qualified")
    ]
;;

let directory_targets_of_rule ~dir { Rule_conf.targets; loc = rule_loc; enabled_if; _ } =
  match targets with
  | Infer ->
    (* we don't infer directory targets *)
    Memo.return Path.Build.Map.empty
  | Static { targets; _ } ->
    let directory_targets =
      List.fold_left targets ~init:Path.Build.Map.empty ~f:(fun acc (target, kind) ->
        match (kind : Targets_spec.Kind.t) with
        | File -> acc
        | Directory ->
          let loc = String_with_vars.loc target in
          (match String_with_vars.text_only target with
           | None ->
             User_error.raise
               ~loc
               [ Pp.text "Variables are not allowed in directory targets." ]
           | Some target ->
             let dir_target = Path.Build.relative ~error_loc:loc dir target in
             if Path.Build.is_descendant dir_target ~of_:dir
             then
               (* We ignore duplicates here as duplicates are detected and
                  reported by [Load_rules]. *)
               Path.Build.Map.set acc dir_target rule_loc
             else
               (* This will be checked when we interpret the stanza
                  completely, so just ignore this rule for now. *)
               acc))
    in
    if Path.Build.Map.is_empty directory_targets
    then Memo.return directory_targets
    else
      (match enabled_if with
       | Blang.Const const -> Memo.return const
       | _ ->
         (* Only evaluate the expander if the enabled_if field is
            non-trivial to avoid memo cycles. If the enabled_if field is absent
            from the "rule" stanza then its value will be [Const true]. *)
         let* expander = Expander0.get ~dir in
         Expander0.eval_blang expander enabled_if)
      >>| (function
       | false -> Path.Build.Map.empty
       | true -> directory_targets)
;;

let extract_directory_targets ~dir stanzas =
  Memo.parallel_map stanzas ~f:(fun stanza ->
    match Stanza.repr stanza with
    | Rule_conf.T rule -> directory_targets_of_rule ~dir rule
    | Coq_stanza.Theory.T m ->
      (* It's unfortunate that we need to pull in the coq rules here. But
         we don't have a generic mechanism for this yet. *)
      Coq_doc.coqdoc_directory_targets ~dir m
    | _ -> Memo.return Path.Build.Map.empty)
  >>| Path.Build.Map.union_all ~f:(fun path loc1 loc2 ->
    User_error.raise
      ~loc:loc1
      [ Pp.textf
          "The following both define the same directory target: %s"
          (Path.Build.to_string path)
      ; Pp.enumerate ~f:Loc.pp_file_colon_line [ loc1; loc2 ]
      ])
;;

module rec DB : sig
  val get : dir:Path.Build.t -> t Memo.t
end = struct
  open DB

  let enclosing_group ~dir =
    match Path.Build.parent dir with
    | None -> Memo.return No_group
    | Some parent_dir -> get ~dir:parent_dir >>| current_group parent_dir
  ;;

  let collect_group =
    let rec walk st_dir ~dir ~local =
      DB.get ~dir
      >>= function
      | Lock_dir | Generated | Source_only _ | Standalone _ | Group_root _ ->
        Memo.return Appendable_list.empty
      | Is_component_of_a_group_but_not_the_root { stanzas; group_root = _ } ->
        let* stanzas =
          match stanzas with
          | None -> Memo.return []
          | Some dune_file -> Dune_file.stanzas dune_file
        in
        walk_children st_dir ~dir ~local
        >>| Appendable_list.( @ )
              (Appendable_list.singleton
                 { Group_component.dir
                 ; path_to_group_root = List.rev local
                 ; source_dir = st_dir
                 ; stanzas
                 })
    and walk_children st_dir ~dir ~local =
      (* TODO take account of directory targets *)
      Source_tree.Dir.sub_dirs st_dir
      |> Filename.Map.to_list
      |> Memo.parallel_map ~f:(fun (basename, st_dir) ->
        let* st_dir = Source_tree.Dir.sub_dir_as_t st_dir in
        let dir = Path.Build.relative dir basename in
        let local = basename :: local in
        walk st_dir ~dir ~local)
      >>| Appendable_list.concat
    in
    fun st_dir ~dir -> walk_children st_dir ~dir ~local:[] >>| Appendable_list.to_list
  ;;

  let has_dune_file ~dir st_dir ~build_dir_is_project_root (d : Dune_file.t) =
    Dune_file.find_stanzas d Include_subdirs.key
    >>| get_include_subdirs
    >>= function
    | Some (loc, Include mode) ->
      let components = Memo.Lazy.create (fun () -> collect_group st_dir ~dir) in
      Memo.return
      @@ T.Group_root
           { source_dir = st_dir
           ; qualification = loc, mode
           ; dune_file = d
           ; components = Memo.Lazy.force components
           }
    | Some (_, No) -> Memo.return (Standalone (st_dir, d))
    | None ->
      if build_dir_is_project_root
      then Memo.return (Standalone (st_dir, d))
      else
        enclosing_group ~dir
        >>= (function
         | No_group -> Memo.return @@ Standalone (st_dir, d)
         | Group_root group_root ->
           let+ () =
             let* stanzas = Dune_file.stanzas d in
             match find_module_stanza stanzas with
             | None -> Memo.return ()
             | Some loc ->
               get ~dir:group_root
               >>| (function
                | Group_root group_root ->
                  error_no_module_consumer ~loc (snd group_root.qualification)
                | _ -> Code_error.raise "impossible as we looked up a group root" [])
           in
           Is_component_of_a_group_but_not_the_root { stanzas = Some d; group_root })
  ;;

  let build_dir_is_project_root st_dir =
    let project_root = Source_tree.Dir.project st_dir |> Dune_project.root in
    Source_tree.Dir.path st_dir |> Path.Source.equal project_root
  ;;

  let get_impl dir =
    (match Path.Build.extract_build_context dir with
     | None -> Memo.return None
     | Some (ctx, dir) ->
       Source_tree.find_dir dir
       >>| (function
        | None -> None
        | Some src_dir -> Some (ctx, src_dir)))
    >>= function
    | None ->
      enclosing_group ~dir
      >>| (function
       | No_group -> Generated
       | Group_root group_root ->
         Is_component_of_a_group_but_not_the_root { stanzas = None; group_root })
    | Some (ctx, st_dir) ->
      let src_dir = Source_tree.Dir.path st_dir in
      Pkg_rules.lock_dir_path (Context_name.of_string ctx)
      >>| (function
             | None -> false
             | Some of_ -> Path.Source.is_descendant ~of_ src_dir)
      >>= (function
       | true -> Memo.return Lock_dir
       | false ->
         let build_dir_is_project_root = build_dir_is_project_root st_dir in
         Dune_load.stanzas_in_dir dir
         >>= (function
          | Some d -> has_dune_file ~dir st_dir ~build_dir_is_project_root d
          | None ->
            if build_dir_is_project_root
            then Memo.return (Source_only st_dir)
            else
              enclosing_group ~dir
              >>| (function
               | No_group -> Source_only st_dir
               | Group_root group_root ->
                 Is_component_of_a_group_but_not_the_root { stanzas = None; group_root })))
  ;;

  let get =
    let memo = Memo.create "get-dir-status" ~input:(module Path.Build) get_impl in
    fun ~dir -> Memo.exec memo dir
  ;;
end

let directory_targets t ~dir =
  match t with
  | Lock_dir | Generated | Source_only _ | Is_component_of_a_group_but_not_the_root _ ->
    Memo.return Path.Build.Map.empty
  | Standalone (_, dune_file) ->
    Dune_file.stanzas dune_file >>= extract_directory_targets ~dir
  | Group_root { components; dune_file; _ } ->
    let f ~dir stanzas acc =
      extract_directory_targets ~dir stanzas >>| Path.Build.Map.superpose acc
    in
    let* init =
      let* stanzas = Dune_file.stanzas dune_file in
      f ~dir stanzas Path.Build.Map.empty
    in
    components
    >>= Memo.List.fold_left ~init ~f:(fun acc { Group_component.dir; stanzas; _ } ->
      f ~dir stanzas acc)
;;
