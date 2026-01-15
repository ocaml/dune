open Import
open Memo.O

type t =
  { project : Dune_project.t
  ; db : Lib.DB.t
  ; coq_db : Coq_lib.DB.t Memo.t
  ; rocq_db : Rocq_lib.DB.t Memo.t
  ; root : Path.Build.t
  }

let root t = t.root
let project t = t.project
let libs t = t.db
let coq_libs t = t.coq_db
let rocq_libs t = t.rocq_db

module DB = struct
  type scope = t
  type t = { by_dir : scope Path.Source.Map.t }

  let find_by_dir t dir = Find_closest_source_dir.find_by_dir_exn t.by_dir ~dir

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
      | Library of
          Library.t
          * Lib_name.t option (* library and optional alias from vendor stanza *)
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
    let resolve_local ~public_libs lib_id_map lib_id =
      match Lib_id.Local.Map.find lib_id_map lib_id with
      | None -> Memo.return Lib.DB.Resolve_result.not_found
      | Some found_or_redirect -> resolve_found_or_redirect ~public_libs found_or_redirect
    in
    fun ~instrument_with ~public_libs ~lib_config stanzas ->
      let by_name, by_id, _ =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Local.Map.empty, Lib_name.Map.empty)
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
              | Library (conf, alias) ->
                let info =
                  let expander = Expander0.get ~dir in
                  let info =
                    Library.to_lib_info conf ~expander ~dir ~lib_config
                    |> Lib_info.of_local
                  in
                  match alias with
                  | None -> info
                  | Some alias -> Lib_info.as_ info ~alias
                and lib_id = Library.to_lib_id ~src_dir conf in
                let name = Option.value alias ~default:(Library.best_name conf) in
                Some lib_id, name, Found_or_redirect.found info
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
              | Some lib_id -> Lib_id.Local.Map.add_exn by_id lib_id r2
            in
            by_name, by_id, libname_conflict_map)
      in
      let resolve name =
        match Lib_name.Map.find by_name name with
        | None | Some [] -> Memo.return []
        | Some [ fr ] -> resolve_found_or_redirect ~public_libs fr >>| List.singleton
        | Some frs -> Memo.parallel_map frs ~f:(resolve_found_or_redirect ~public_libs)
      and resolve_local = resolve_local ~public_libs by_id in
      Lib.DB.create
        ()
        ~parent:(Some public_libs)
        ~resolve
        ~resolve_local
        ~resolve_external:(fun _ -> Memo.return Lib.DB.Resolve_result.not_found)
        ~all:(fun () -> Lib_name.Map.keys by_name |> Memo.return)
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
    let resolve_local t local_libs lib_id =
      match Lib_id.Local.Map.find local_libs lib_id with
      | None -> Memo.return Lib.DB.Resolve_result.not_found
      | Some rt -> resolve_redirect_to t rt
    in
    fun t ~installed_libs stanzas ->
      let by_name, by_id =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Local.Map.empty)
          ~f:
            (fun
              (by_name, by_id)
              ((dir, stanza) : Path.Build.t * Library_related_stanza.t)
            ->
            let candidate =
              match stanza with
              | Library (({ project; visibility = Public p; _ } as conf), alias) ->
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
                let name = Option.value alias ~default:(Public_lib.name p) in
                Some
                  ( name
                  , Project { project; lib_id; enabled; loc = Public_lib.loc p }
                  , Some lib_id )
              | Library (_, _) | Library_redirect _ -> None
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
                | Some lib_id2 -> Lib_id.Local.Map.add_exn by_id lib_id2 r2
              in
              by_name, by_id)
      in
      let resolve_local = resolve_local t by_id in
      let resolve name =
        match Lib_name.Map.find by_name name with
        | None -> Memo.return []
        | Some rt -> Memo.List.map ~f:(resolve_redirect_to t) rt
      in
      Lib.DB.create
        ~parent:(Some installed_libs)
        ~resolve
        ~resolve_local
        ~resolve_external:(fun _ -> Memo.return Lib.DB.Resolve_result.not_found)
        ~all:(fun () -> Lib_name.Map.keys by_name |> Memo.return)
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
        rocq_stanzas
    =
    let stanzas_by_project_dir =
      List.map stanzas ~f:(fun (dir, stanza) ->
        let project =
          match (stanza : Library_related_stanza.t) with
          | Library (lib, _) -> lib.project
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
    let rocq_scopes =
      Rocq_scope.make
        context
        ~public_libs
        rocq_stanzas
        ~db_by_project_dir
        ~projects_by_root
    in
    Path.Source.Map.mapi db_by_project_dir ~f:(fun dir (project, db) ->
      let root = Path.Build.append_source build_dir (Dune_project.root project) in
      let coq_db = Coq_scope.find coq_scopes ~dir in
      let rocq_db = Rocq_scope.find rocq_scopes ~dir in
      { project; db; coq_db; rocq_db; root })
  ;;

  let create ~context ~projects_by_root stanzas coq_stanzas rocq_stanzas =
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
      public_libs t ~instrument_with ~installed_libs stanzas
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
        rocq_stanzas
    in
    let value = { by_dir } in
    Fdecl.set t value;
    value, public_libs
  ;;

  let create_from_stanzas ~projects_by_root ~(context : Context_name.t) stanzas =
    let build_dir = Context_name.build_dir context in
    (* Collect all stanzas, checking vendor stanza filtering for libraries *)
    let* stanzas, coq_stanzas, rocq_stanzas =
      Dune_file.Memo_fold.fold_static_stanzas
        stanzas
        ~init:([], [], [])
        ~f:(fun dune_file stanza (acc, coq_acc, rocq_acc) ->
          let src_dir = Dune_file.dir dune_file in
          let ctx_dir = Path.Build.append_source build_dir src_dir in
          match Stanza.repr stanza with
          | Library.T lib ->
            (* Check if this library is filtered by a vendor stanza and get alias.
               Use the LOCAL name (lib.name) for matching against vendor stanzas,
               since that's how users specify libraries in vendor stanzas. *)
            let lib_name = Library.best_name lib in
            let lib_pkg = Option.map (Library.package lib) ~f:Package.name in
            let+ status = Lib.library_status ~src_dir ~lib_name ~lib_pkg in
            (match status with
             | `Excluded -> acc, coq_acc, rocq_acc
             | `Included alias ->
               ( (ctx_dir, Library_related_stanza.Library (lib, alias)) :: acc
               , coq_acc
               , rocq_acc ))
          | Deprecated_library_name.T d ->
            Memo.return
              ( (ctx_dir, Library_related_stanza.Deprecated_library_name d) :: acc
              , coq_acc
              , rocq_acc )
          | Library_redirect.Local.T d ->
            Memo.return
              ( (ctx_dir, Library_related_stanza.Library_redirect d) :: acc
              , coq_acc
              , rocq_acc )
          | Coq_stanza.Theory.T coq_lib ->
            Memo.return (acc, (ctx_dir, coq_lib) :: coq_acc, rocq_acc)
          | Rocq_stanza.Theory.T rocq_lib ->
            Memo.return (acc, coq_acc, (ctx_dir, rocq_lib) :: rocq_acc)
          | _ -> Memo.return (acc, coq_acc, rocq_acc))
    in
    create ~projects_by_root ~context stanzas coq_stanzas rocq_stanzas
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

    module Set = struct
      type t =
        { libraries : Lib.Local.t list
        ; deprecated_library_names : Deprecated_library_name.t list
        }

      let empty = { libraries = []; deprecated_library_names = [] }

      let of_list =
        let by_name x = Lib.Local.info x |> Lib_info.name in
        fun xs ->
          let libraries, deprecated_library_names =
            List.partition_map xs ~f:(function
              | Library l -> Left l
              | Deprecated_library_name l -> Right l)
          in
          { libraries =
              List.sort libraries ~compare:(fun x y ->
                Lib_name.compare (by_name x) (by_name y))
          ; deprecated_library_names =
              List.sort
                deprecated_library_names
                ~compare:(fun { old_name = old_public_name, _; _ } y ->
                  Lib_name.compare
                    (Public_lib.name old_public_name)
                    (Public_lib.name (fst y.old_name)))
          }
      ;;

      let fold { libraries; deprecated_library_names } ~init ~f =
        let init =
          List.fold_left ~init libraries ~f:(fun acc lib -> f (Library lib) acc)
        in
        List.fold_left deprecated_library_names ~init ~f:(fun acc dep ->
          f (Deprecated_library_name dep) acc)
      ;;

      let partition_map t ~f =
        let l, r =
          fold t ~init:([], []) ~f:(fun x (l, r) ->
            match f x with
            | Left x -> x :: l, r
            | Right x -> l, x :: r)
        in
        List.(rev l, rev r)
      ;;
    end
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
              (* Check vendor stanza filtering *)
              let src_dir = Dune_file.dir d in
              let lib_name = Library.best_name lib in
              let lib_pkg = Option.map (Library.package lib) ~f:Package.name in
              let* status = Lib.library_status ~src_dir ~lib_name ~lib_pkg in
              match status with
              | `Excluded -> Memo.return acc
              | `Included _ ->
                (match lib.visibility with
                 | Private None -> Memo.return acc
                 | Private (Some pkg) ->
                   let src_dir = Dune_file.dir d in
                   let* scope =
                     find_by_dir (Path.Build.append_source build_dir src_dir)
                   in
                   Lib.DB.find_lib_id
                     (libs scope)
                     (Local (Library.to_lib_id ~src_dir lib))
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
                      (name, Lib_entry.Library local_lib) :: acc)))
          | Deprecated_library_name.T ({ old_name = old_public_name, _; _ } as d) ->
            let package = Public_lib.package old_public_name in
            let name = Package.name package in
            Memo.return ((name, Lib_entry.Deprecated_library_name d) :: acc)
          | _ -> Memo.return acc)
      in
      Package.Name.Map.of_list_multi libs |> Package.Name.Map.map ~f:Lib_entry.Set.of_list
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
      match Package.Name.Map.find map pkg_name with
      | None -> Lib_entry.Set.empty
      | Some libs ->
        let _by_name =
          Lib_entry.Set.fold libs ~init:Lib_name.Map.empty ~f:(fun entry2 by_name ->
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
