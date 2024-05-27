open Import
open Memo.O

type t =
  { project : Dune_project.t
  ; db : Lib.DB.t
  ; coq_db : Coq_lib.DB.t Memo.t
  ; root : Path.Build.t
  }

let root t = t.root
let project t = t.project
let libs t = t.db
let coq_libs t = t.coq_db

module DB = struct
  type scope = t
  type t = { by_dir : scope Path.Source.Map.t }

  let find_by_dir t dir = Find_closest_source_dir.find_by_dir t.by_dir ~dir

  let find_by_project t project =
    Path.Source.Map.find_exn t.by_dir (Dune_project.root project)
  ;;

  module Found_or_redirect : sig
    type t = private
      | Found of Lib_info.external_
      | Redirect of
          { loc : Loc.t
          ; to_ : Lib_name.t
          ; enabled : Toggle.t Memo.t
          }
      | Deprecated_library_name of (Loc.t * Lib_name.t)

    val redirect
      :  enabled:Toggle.t Memo.t
      -> Lib_name.t
      -> Loc.t * Lib_name.t
      -> Lib_name.t * t

    val deprecated_library_name : Lib_name.t -> Loc.t * Lib_name.t -> Lib_name.t * t
    val found : Lib_info.external_ -> t
  end = struct
    type t =
      | Found of Lib_info.external_
      | Redirect of
          { loc : Loc.t
          ; to_ : Lib_name.t
          ; enabled : Toggle.t Memo.t
          }
      | Deprecated_library_name of (Loc.t * Lib_name.t)

    let redirect ~enabled from (loc, to_) =
      if Lib_name.equal from to_
      then Code_error.raise ~loc "Invalid redirect" [ "to_", Lib_name.to_dyn to_ ]
      else from, Redirect { loc; to_; enabled }
    ;;

    let deprecated_library_name from (loc, to_) =
      if Lib_name.equal from to_
      then Code_error.raise ~loc "Invalid redirect" [ "to_", Lib_name.to_dyn to_ ]
      else from, Deprecated_library_name (loc, to_)
    ;;

    let found x = Found x
  end

  module Library_related_stanza = struct
    type t =
      | Library of Library.t
      | Library_redirect of Library_redirect.Local.t
      | Deprecated_library_name of Deprecated_library_name.t
  end

  let create_db_from_stanzas =
    (* Here, [parent] is always the public_libs DB. Check the call to
       [create_db_from_stanzas] below. *)
    let resolve_found_or_redirect ~public_libs fr =
      match (fr : Found_or_redirect.t) with
      | Redirect { loc; to_; enabled; _ } ->
        let+ enabled =
          let+ toggle = enabled in
          Toggle.enabled toggle
        in
        if enabled
        then Lib.DB.Resolve_result.redirect_in_the_same_db (loc, to_)
        else Lib.DB.Resolve_result.not_found
      | Found lib -> Memo.return (Lib.DB.Resolve_result.found lib)
      | Deprecated_library_name lib ->
        Memo.return (Lib.DB.Resolve_result.redirect_by_name public_libs lib)
    in
    let resolve_lib_id ~public_libs lib_id_map lib_id =
      match Lib_id.Map.find lib_id_map lib_id with
      | None -> Memo.return Lib.DB.Resolve_result.not_found
      | Some found_or_redirect -> resolve_found_or_redirect ~public_libs found_or_redirect
    in
    fun ~instrument_with ~public_libs ~lib_config stanzas ->
      let by_name, by_id, _ =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Map.empty, Lib_name.Map.empty)
          ~f:(fun (by_name, by_id, libname_conflict_map) (dir, stanza) ->
            let lib_id, name, r2 =
              let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
              match (stanza : Library_related_stanza.t) with
              | Library_redirect s ->
                let lib_name, redirect =
                  let old_public_name = Lib_name.of_local s.old_name.lib_name in
                  let enabled =
                    Memo.lazy_ (fun () ->
                      let* expander = Expander0.get ~dir in
                      Expander0.eval_blang expander s.old_name.enabled >>| Toggle.of_bool)
                    |> Memo.Lazy.force
                  in
                  Found_or_redirect.redirect ~enabled old_public_name s.new_public_name
                and lib_id = Library_redirect.Local.to_lib_id ~src_dir s in
                Some lib_id, lib_name, redirect
              | Deprecated_library_name s ->
                let lib_name, deprecated_lib =
                  let old_public_name = Deprecated_library_name.old_public_name s in
                  Found_or_redirect.deprecated_library_name
                    old_public_name
                    s.new_public_name
                in
                None, lib_name, deprecated_lib
              | Library (conf : Library.t) ->
                let info =
                  let expander = Expander0.get ~dir in
                  Library.to_lib_info conf ~expander ~dir ~lib_config |> Lib_info.of_local
                and lib_id = Library.to_lib_id ~src_dir conf in
                Some lib_id, Library.best_name conf, Found_or_redirect.found info
            in
            let libname_conflict_map =
              Lib_name.Map.update libname_conflict_map name ~f:(function
                | None -> Some r2
                | Some (r1 : Found_or_redirect.t) ->
                  let res =
                    match r1, r2 with
                    | Found _, Found _
                    | Found _, Redirect _
                    | Redirect _, Found _
                    | Redirect _, Redirect _ -> Ok r1
                    | Found info, Deprecated_library_name (loc, _)
                    | Deprecated_library_name (loc, _), Found info ->
                      Error (loc, Lib_info.loc info)
                    | ( Deprecated_library_name (loc2, lib2)
                      , Redirect { loc = loc1; to_ = lib1; _ } )
                    | ( Redirect { loc = loc1; to_ = lib1; _ }
                      , Deprecated_library_name (loc2, lib2) )
                    | ( Deprecated_library_name (loc1, lib1)
                      , Deprecated_library_name (loc2, lib2) ) ->
                      if Lib_name.equal lib1 lib2 then Ok r1 else Error (loc1, loc2)
                  in
                  (match res with
                   | Ok x -> Some x
                   | Error (loc1, loc2) ->
                     let main_message =
                       Pp.textf "Library %s is defined twice:" (Lib_name.to_string name)
                     in
                     let annots =
                       let main = User_message.make ~loc:loc2 [ main_message ] in
                       let related =
                         [ User_message.make ~loc:loc1 [ Pp.text "Already defined here" ]
                         ]
                       in
                       User_message.Annots.singleton
                         Compound_user_error.annot
                         [ Compound_user_error.make ~main ~related ]
                     in
                     User_error.raise
                       ~annots
                       [ main_message
                       ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
                       ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
                       ]))
            in
            let by_name =
              Lib_name.Map.update by_name name ~f:(function
                | None -> Some [ r2 ]
                | Some rest -> Some (r2 :: rest))
            and by_id =
              match lib_id with
              | None -> by_id
              | Some lib_id -> Lib_id.Map.add_exn by_id (Local lib_id) r2
            in
            by_name, by_id, libname_conflict_map)
      in
      let resolve name =
        match Lib_name.Map.find by_name name with
        | None | Some [] -> Memo.return []
        | Some [ fr ] -> resolve_found_or_redirect ~public_libs fr >>| List.singleton
        | Some frs -> Memo.parallel_map frs ~f:(resolve_found_or_redirect ~public_libs)
      and resolve_lib_id = resolve_lib_id ~public_libs by_id in
      Lib.DB.create
        ()
        ~parent:(Some public_libs)
        ~resolve
        ~resolve_lib_id
        ~all:(fun () -> Lib_name.Map.keys by_name |> Memo.return)
        ~lib_config
        ~instrument_with
  ;;

  type redirect_to =
    | Project of
        { project : Dune_project.t
        ; lib_id : Lib_id.Local.t
        ; enabled : Toggle.t Memo.t
        ; loc : Loc.t
        }
    | Name of (Loc.t * Lib_name.t)

  (* Create a database from the public libraries defined in the stanzas *)
  let public_libs =
    let resolve_redirect_to t rt =
      match rt with
      | Project { project; lib_id; enabled; _ } ->
        let+ enabled =
          let+ toggle = enabled in
          Toggle.enabled toggle
        in
        if enabled
        then (
          let scope = find_by_project (Fdecl.get t) project in
          Lib.DB.Resolve_result.redirect_by_id scope.db (Local lib_id))
        else Lib.DB.Resolve_result.not_found
      | Name name -> Memo.return (Lib.DB.Resolve_result.redirect_in_the_same_db name)
    in
    let resolve_lib_id t public_libs lib_id =
      match Lib_id.Map.find public_libs lib_id with
      | None -> Memo.return Lib.DB.Resolve_result.not_found
      | Some rt -> resolve_redirect_to t rt
    in
    fun t ~installed_libs ~lib_config stanzas ->
      let by_name, by_id =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Map.empty)
          ~f:
            (fun
              (by_name, by_id)
              ((dir, stanza) : Path.Build.t * Library_related_stanza.t)
            ->
            let candidate =
              match stanza with
              | Library ({ project; visibility = Public p; _ } as conf) ->
                let lib_id =
                  let src_dir =
                    Path.drop_optional_build_context_src_exn (Path.build dir)
                  in
                  Library.to_lib_id ~src_dir conf
                in
                let enabled =
                  Memo.lazy_ (fun () ->
                    let* expander = Expander0.get ~dir in
                    Expander0.eval_blang expander conf.enabled_if >>| Toggle.of_bool)
                  |> Memo.Lazy.force
                in
                Some
                  ( Public_lib.name p
                  , Project { project; lib_id; enabled; loc = Public_lib.loc p }
                  , Some lib_id )
              | Library _ | Library_redirect _ -> None
              | Deprecated_library_name s ->
                Some
                  (Deprecated_library_name.old_public_name s, Name s.new_public_name, None)
            in
            match candidate with
            | None -> by_name, by_id
            | Some (public_name, r2, lib_id2) ->
              let by_name =
                Lib_name.Map.update by_name public_name ~f:(function
                  | None -> Some [ r2 ]
                  | Some r1 -> Some (r2 :: r1))
              in
              let by_id =
                match lib_id2 with
                | None -> by_id
                | Some lib_id2 -> Lib_id.Map.add_exn by_id (Local lib_id2) r2
              in
              by_name, by_id)
      in
      let resolve_lib_id lib_id = resolve_lib_id t by_id lib_id in
      let resolve name =
        match Lib_name.Map.find by_name name with
        | None -> Memo.return []
        | Some rt -> Memo.List.map ~f:(resolve_redirect_to t) rt
      in
      Lib.DB.create
        ~parent:(Some installed_libs)
        ~resolve
        ~resolve_lib_id
        ~all:(fun () -> Lib_name.Map.keys by_name |> Memo.return)
        ~lib_config
        ()
  ;;

  module Path_source_map_traversals = Memo.Make_parallel_map (Path.Source.Map)

  let scopes_by_dir
    ~build_dir
    ~lib_config
    ~projects_by_root
    ~public_libs
    ~instrument_with
    context
    stanzas
    coq_stanzas
    =
    let stanzas_by_project_dir =
      List.map stanzas ~f:(fun (dir, stanza) ->
        let project =
          match (stanza : Library_related_stanza.t) with
          | Library lib -> lib.project
          | Library_redirect x -> x.project
          | Deprecated_library_name x -> x.project
        in
        Dune_project.root project, (dir, stanza))
      |> Path.Source.Map.of_list_multi
    in
    let db_by_project_dir =
      Path.Source.Map.merge
        projects_by_root
        stanzas_by_project_dir
        ~f:(fun _dir project stanzas ->
          let project = Option.value_exn project in
          let stanzas = Option.value stanzas ~default:[] in
          Some (project, stanzas))
      |> Path.Source.Map.map ~f:(fun (project, stanzas) ->
        let db =
          create_db_from_stanzas stanzas ~instrument_with ~public_libs ~lib_config
        in
        project, db)
    in
    let coq_scopes =
      Coq_scope.make context ~public_libs coq_stanzas ~db_by_project_dir ~projects_by_root
    in
    Path.Source.Map.mapi db_by_project_dir ~f:(fun dir (project, db) ->
      let root = Path.Build.append_source build_dir (Dune_project.root project) in
      let coq_db = Coq_scope.find coq_scopes ~dir in
      { project; db; coq_db; root })
  ;;

  let create ~context ~projects_by_root stanzas coq_stanzas =
    let open Memo.O in
    let t = Fdecl.create Dyn.opaque in
    let* context = Context.DB.get context in
    let build_dir = Context.build_dir context in
    let* lib_config =
      let+ ocaml = Context.ocaml context in
      ocaml.lib_config
    in
    let instrument_with = Context.instrument_with context in
    let+ public_libs =
      let+ installed_libs = Lib.DB.installed context in
      public_libs t ~instrument_with ~lib_config ~installed_libs stanzas
    in
    let by_dir =
      scopes_by_dir
        ~build_dir
        ~lib_config
        ~projects_by_root
        ~public_libs
        ~instrument_with
        context
        stanzas
        coq_stanzas
    in
    let value = { by_dir } in
    Fdecl.set t value;
    value, public_libs
  ;;

  let create_from_stanzas ~projects_by_root ~(context : Context_name.t) stanzas =
    let stanzas, coq_stanzas =
      let build_dir = Context_name.build_dir context in
      Dune_file.fold_static_stanzas
        stanzas
        ~init:([], [])
        ~f:(fun dune_file stanza (acc, coq_acc) ->
          match Stanza.repr stanza with
          | Library.T lib ->
            let ctx_dir = Path.Build.append_source build_dir (Dune_file.dir dune_file) in
            (ctx_dir, Library_related_stanza.Library lib) :: acc, coq_acc
          | Deprecated_library_name.T d ->
            let ctx_dir = Path.Build.append_source build_dir (Dune_file.dir dune_file) in
            (ctx_dir, Deprecated_library_name d) :: acc, coq_acc
          | Library_redirect.Local.T d ->
            let ctx_dir = Path.Build.append_source build_dir (Dune_file.dir dune_file) in
            (ctx_dir, Library_redirect d) :: acc, coq_acc
          | Coq_stanza.Theory.T coq_lib ->
            let ctx_dir = Path.Build.append_source build_dir (Dune_file.dir dune_file) in
            acc, (ctx_dir, coq_lib) :: coq_acc
          | _ -> acc, coq_acc)
    in
    create ~projects_by_root ~context stanzas coq_stanzas
  ;;

  let all =
    Per_context.create_by_name ~name:"scope" (fun context ->
      Memo.Lazy.create (fun () ->
        let* projects_by_root = Dune_load.projects_by_root ()
        and* stanzas = Dune_load.dune_files context in
        create_from_stanzas ~projects_by_root ~context stanzas)
      |> Memo.Lazy.force)
    |> Staged.unstage
  ;;

  let create_from_stanzas (context : Context_name.t) = all context

  let with_all context ~f =
    let+ scopes, _ = create_from_stanzas (Context.name context) in
    let find = find_by_project scopes in
    f find
  ;;

  let public_libs context =
    let+ _, public_libs = create_from_stanzas context in
    public_libs
  ;;

  let find_by_dir dir =
    let* context = Context.DB.by_dir dir in
    let+ scopes, _ = create_from_stanzas (Context.name context) in
    find_by_dir scopes dir
  ;;

  let find_by_project context project =
    let+ scopes, _ = create_from_stanzas context in
    find_by_project scopes project
  ;;

  module Lib_entry = struct
    type t =
      | Library of Lib.Local.t
      | Deprecated_library_name of Deprecated_library_name.t

    let name = function
      | Library lib -> Lib.Local.to_lib lib |> Lib.name
      | Deprecated_library_name { old_name = old_public_name, _; _ } ->
        Public_lib.name old_public_name
    ;;

    let loc = function
      | Library lib -> Lib.Local.to_lib lib |> Lib.info |> Lib_info.loc
      | Deprecated_library_name { old_name = old_public_name, _; _ } ->
        Public_lib.loc old_public_name
    ;;
  end

  let lib_entries_of_package =
    let make_map build_dir public_libs stanzas =
      let+ libs =
        Dune_file.Memo_fold.fold_static_stanzas stanzas ~init:[] ~f:(fun d stanza acc ->
          match Stanza.repr stanza with
          | Library.T ({ enabled_if; _ } as lib) ->
            let* enabled =
              let* expander = Expander0.get ~dir:build_dir in
              Expander0.eval_blang expander enabled_if
            in
            if not enabled
            then Memo.return acc
            else (
              match lib.visibility with
              | Private None -> Memo.return acc
              | Private (Some pkg) ->
                let src_dir = Dune_file.dir d in
                let* scope = find_by_dir (Path.Build.append_source build_dir src_dir) in
                Lib.DB.find_lib_id (libs scope) (Local (Library.to_lib_id ~src_dir lib))
                >>| (function
                 | None -> acc
                 | Some lib ->
                   let name = Package.name pkg in
                   (name, Lib_entry.Library (Lib.Local.of_lib_exn lib)) :: acc)
              | Public pub ->
                let src_dir = Dune_file.dir d in
                Lib.DB.find_lib_id public_libs (Local (Library.to_lib_id ~src_dir lib))
                >>| (function
                 | None -> acc
                 | Some lib ->
                   let package = Public_lib.package pub in
                   let name = Package.name package in
                   let local_lib = Lib.Local.of_lib_exn lib in
                   (name, Lib_entry.Library local_lib) :: acc))
          | Deprecated_library_name.T ({ old_name = old_public_name, _; _ } as d) ->
            let package = Public_lib.package old_public_name in
            let name = Package.name package in
            Memo.return ((name, Lib_entry.Deprecated_library_name d) :: acc)
          | _ -> Memo.return acc)
      in
      Package.Name.Map.of_list_multi libs
      |> Package.Name.Map.map
           ~f:
             (List.sort ~compare:(fun a b ->
                Lib_name.compare (Lib_entry.name a) (Lib_entry.name b)))
    in
    let per_context =
      Per_context.create_by_name ~name:"scope-db" (fun ctx ->
        Memo.lazy_ (fun () ->
          let* public_libs =
            let* ctx = Context.DB.get ctx in
            public_libs (Context.name ctx)
          and* stanzas = Dune_load.dune_files ctx in
          make_map (Context_name.build_dir ctx) public_libs stanzas)
        |> Memo.Lazy.force)
      |> Staged.unstage
    in
    fun (ctx : Context_name.t) pkg_name ->
      let+ map = per_context ctx in
      match Package.Name.Map.Multi.find map pkg_name with
      | ([] | [ _ ]) as xs -> xs
      | libs ->
        let _by_name =
          List.fold_left libs ~init:Lib_name.Map.empty ~f:(fun by_name entry2 ->
            let public_name = Lib_entry.name entry2 in
            Lib_name.Map.update by_name public_name ~f:(function
              | None -> Some entry2
              | Some entry1 ->
                let loc1 = Lib_entry.loc entry1
                and loc2 = Lib_entry.loc entry2 in
                let main_message =
                  Pp.textf
                    "Public library %s is defined twice:"
                    (Lib_name.to_string public_name)
                in
                let annots =
                  let main = User_message.make ~loc:loc2 [ main_message ] in
                  let related =
                    [ User_message.make ~loc:loc1 [ Pp.text "Already defined here" ] ]
                  in
                  User_message.Annots.singleton
                    Compound_user_error.annot
                    [ Compound_user_error.make ~main ~related ]
                in
                User_error.raise
                  ~annots
                  ~loc:loc2
                  [ main_message
                  ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
                  ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
                  ]))
        in
        libs
  ;;
end
