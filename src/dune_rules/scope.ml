open Import
open Memo.O

type t =
  { loaded_project : Loaded_project.t
  ; project : Dune_project.t
  ; db : Lib.DB.t
  ; public_libs : Lib.DB.t
  ; rocq_db : Rocq_lib.DB.t Memo.t
  ; root : Path.Build.t
  }

let root t = t.root
let project t = t.project
let loaded_project t = t.loaded_project
let source_dir t dir = Loaded_project.source_path t.loaded_project dir |> Option.value_exn
let libs t = t.db
let public_libs t = t.public_libs
let rocq_libs t = t.rocq_db

module DB = struct
  type scope = t

  type t =
    { by_dir : scope Path.Build.Map.t
    ; by_package : scope Package.Name.Map.t
    ; dependency_libs_by_package : Lib.DB.t Package.Name.Map.t
    }

  let find_by_dir t dir =
    let rec loop dir =
      match Path.Build.Map.find t.by_dir dir with
      | Some scope -> scope
      | None ->
        (match Path.Build.parent dir with
         | Some parent -> loop parent
         | None ->
           Code_error.raise
             "Scope: no enclosing loaded project"
             [ "dir", Path.Build.to_dyn dir ])
    in
    let scope = loop dir in
    let src_dir = source_dir scope dir in
    match Dune_project.exclusive_package scope.project ~dir:src_dir with
    | None -> scope
    | Some package -> Package.Name.Map.find_exn t.by_package (Package.Id.name package)
  ;;

  let find_by_project t project =
    Path.Build.Map.values t.by_dir
    |> List.find_exn ~f:(fun scope -> Dune_project.equal scope.project project)
  ;;

  module Found_or_redirect : sig
    type t = private
      | Found of
          { info : Lib_info.external_
          ; owner : Package.Name.t option
          }
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
    val found : Lib_info.external_ -> owner:Package.Name.t option -> t
  end = struct
    type t =
      | Found of
          { info : Lib_info.external_
          ; owner : Package.Name.t option
          }
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

    let found info ~owner = Found { info; owner }
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
    let resolve_found_or_redirect ~scopes ~project ~owner ~public_libs fr =
      match (fr : Found_or_redirect.t) with
      | Redirect { loc; to_; enabled; _ } ->
        let+ enabled =
          let+ toggle = enabled in
          Toggle.enabled toggle
        in
        if enabled
        then Lib.DB.Resolve_result.redirect_in_the_same_db (loc, to_)
        else Lib.DB.Resolve_result.not_found
      | Found { info; owner = library_owner } ->
        let result =
          if Option.equal Package.Name.equal owner library_owner
          then Lib.DB.Resolve_result.found info
          else (
            let scopes = Fdecl.get scopes in
            let scope =
              match library_owner with
              | None -> find_by_project scopes project
              | Some package -> Package.Name.Map.find_exn scopes.by_package package
            in
            Lib.DB.Resolve_result.redirect_by_id scope.db (Lib_info.lib_id info))
        in
        Memo.return result
      | Deprecated_library_name lib ->
        Memo.return (Lib.DB.Resolve_result.redirect_by_name public_libs lib)
    in
    fun ~lib_config ~project stanzas ->
      let by_name, by_id, _ =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Map.empty, Lib_name.Map.empty)
          ~f:(fun (by_name, by_id, libname_conflict_map) (loaded_dir, stanza) ->
            let dir = Loaded_dir.output_dir loaded_dir in
            let src_dir = Loaded_dir.source_dir loaded_dir in
            let lib_id, name, r2 =
              match (stanza : Library_related_stanza.t) with
              | Library_redirect s ->
                let lib_name, redirect =
                  let old_public_name = Lib_name.of_local s.old_name.lib_name in
                  let enabled =
                    Memo.lazy_ ~name:"library-redirect-enabled" (fun () ->
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
                  Library.to_lib_info conf ~expander ~dir ~src_dir ~lib_config
                  |> Lib_info.of_local
                and lib_id = Library.to_lib_id ~src_dir conf in
                let owner =
                  match Library.package conf with
                  | Some package -> Some (Package.name package)
                  | None ->
                    Dune_project.exclusive_package project ~dir:src_dir
                    |> Option.map ~f:Package.Id.name
                in
                Some lib_id, Library.best_name conf, Found_or_redirect.found info ~owner
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
                    | Found { info; _ }, Deprecated_library_name (loc, _)
                    | Deprecated_library_name (loc, _), Found { info; _ } ->
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
                     let compound =
                       Compound_user_error.duplicate
                         ~main_loc:loc2
                         ~previous_loc:loc1
                         main_message
                     in
                     User_error.raise
                       ~compound
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
      Staged.stage (fun ~scopes ~owner ~public_libs ~instrument_with ~package_scopes ->
        let resolve_found_or_redirect =
          resolve_found_or_redirect ~scopes ~project ~owner ~public_libs
        in
        let resolve name =
          match Lib_name.Map.find by_name name with
          | None | Some [] -> Memo.return []
          | Some [ fr ] -> resolve_found_or_redirect fr >>| List.singleton
          | Some frs -> Memo.parallel_map frs ~f:resolve_found_or_redirect
        and resolve_lib_id lib_id =
          match Lib_id.Map.find by_id lib_id with
          | None -> Memo.return Lib.DB.Resolve_result.not_found
          | Some fr -> resolve_found_or_redirect fr
        in
        Lib.DB.create
          ()
          ~parent:(Some public_libs)
          ~resolve
          ~resolve_lib_id
          ~all:(fun () -> Lib_name.Map.keys by_name |> Memo.return)
          ~instrument_with
        |> Lib.DB.with_package_scopes ~package_scopes)
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
    fun t ~context ~installed_libs ~instrument_with stanzas ->
      let opam_libraries =
        Memo.create
          "opam-package-libraries"
          ~input:(module Package.Name)
          (fun name ->
             let* package = Opam_package.find context name in
             match package with
             | None -> Memo.return None
             | Some package ->
               let* dependency, _ =
                 Action_builder.evaluate_and_collect_facts
                   (Opam_package.dependency package)
               in
               let roots = Opam_build.Paths.install_roots dependency.paths in
               let* findlib =
                 Findlib.create_for_package context ~package:name ~lib_root:roots.lib_root
               in
               let resolve library =
                 let+ entry = Findlib.find findlib library in
                 Lib.DB.Resolve_result.of_findlib entry
               in
               let parent =
                 Package.Name.Map.find_exn (Fdecl.get t).dependency_libs_by_package name
               in
               let db =
                 Lib.DB.create
                   ()
                   ~parent:(Some parent)
                   ~resolve:(fun name -> resolve name >>| List.singleton)
                   ~resolve_lib_id:(fun id -> resolve (Lib_id.name id))
                   ~all:(fun () ->
                     Findlib.all_packages findlib >>| List.map ~f:Dune_package.Entry.name)
                   ~instrument_with
               in
               Memo.return (Some (findlib, db)))
        |> Memo.exec
      in
      let by_name, by_id =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Map.empty)
          ~f:
            (fun
              (by_name, by_id)
              ((loaded_dir, stanza) : Loaded_dir.t * Library_related_stanza.t)
            ->
            let dir = Loaded_dir.output_dir loaded_dir in
            let candidate =
              match stanza with
              | Library ({ project; visibility = Public p; _ } as conf) ->
                let lib_id =
                  let src_dir = Loaded_dir.source_dir loaded_dir in
                  Library.to_lib_id ~src_dir conf
                in
                let enabled =
                  Memo.lazy_ ~name:"library-enabled" (fun () ->
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
      Staged.stage (fun ~owner ~include_owner ->
        let candidates =
          Memo.lazy_ ~name:"opaque-library-candidates" (fun () ->
            let* packages = Opam_package.all context in
            match owner with
            | None -> Memo.return (Package.Name.Map.keys packages)
            | Some owner ->
              let+ visible =
                Dune_load.package_closure (Package.Name.Set.singleton owner)
              in
              let visible =
                if include_owner then visible else Package.Name.Set.remove visible owner
              in
              Package.Name.Map.keys packages
              |> List.filter ~f:(Package.Name.Set.mem visible))
        in
        let resolve_opam name =
          let* candidates = Memo.Lazy.force candidates in
          Memo.parallel_map candidates ~f:(fun package ->
            let* libraries = opam_libraries package in
            match libraries with
            | None -> Memo.return None
            | Some (findlib, db) ->
              let+ entry = Findlib.find findlib name in
              (match entry with
               | Error Not_found -> None
               | Ok _ | Error (Invalid_dune_package _) ->
                 Some (Lib.DB.Resolve_result.redirect_by_name db (Loc.none, name))))
          >>| List.filter_opt
        in
        let all_opam () =
          let* candidates = Memo.Lazy.force candidates in
          Memo.List.concat_map candidates ~f:(fun name ->
            let* libraries = opam_libraries name in
            match libraries with
            | None -> Memo.return []
            | Some (findlib, _) ->
              Findlib.all_packages findlib >>| List.map ~f:Dune_package.Entry.name)
        in
        let opaque_libs =
          Lib.DB.create
            ()
            ~parent:(Some installed_libs)
            ~resolve:resolve_opam
            ~resolve_lib_id:(fun lib_id ->
              Memo.return
                (Lib.DB.Resolve_result.redirect_in_the_same_db
                   (Loc.none, Lib_id.name lib_id)))
            ~all:all_opam
            ~instrument_with
        in
        let resolve_lib_id lib_id =
          match Lib_id.Map.find by_id lib_id with
          | None -> Memo.return Lib.DB.Resolve_result.not_found
          | Some rt -> resolve_redirect_to t rt
        in
        let resolve name =
          match Lib_name.Map.find by_name name with
          | None -> Memo.return []
          | Some rt -> Memo.List.map ~f:(resolve_redirect_to t) rt
        in
        Lib.DB.create
          ~parent:(Some opaque_libs)
          ~resolve
          ~resolve_lib_id
          ~all:(fun () ->
            let+ libraries = all_opam () in
            List.sort_uniq
              (Lib_name.Map.keys by_name @ libraries)
              ~compare:Lib_name.compare)
          ~instrument_with
          ())
  ;;

  let scopes_by_dir
        ~scopes
        ~package_scopes
        ~lib_config
        ~projects_by_root
        ~public_libs
        ~public_libs_by_package
        ~dependency_libs_by_package
        ~instrument_with
        context
        stanzas
        rocq_stanzas
    =
    let stanzas_by_project_dir =
      List.map stanzas ~f:(fun (dir, stanza) ->
        Loaded_project.output_root (Loaded_dir.project dir), (dir, stanza))
      |> Path.Build.Map.of_list_multi
    in
    let make_db_by_project_dir =
      Path.Build.Map.merge
        projects_by_root
        stanzas_by_project_dir
        ~f:(fun _dir project stanzas ->
          let project = Option.value_exn project in
          let stanzas = Option.value stanzas ~default:[] in
          Some (project, stanzas))
      |> Path.Build.Map.map ~f:(fun (loaded_project, stanzas) ->
        let project = Loaded_project.project loaded_project in
        ( loaded_project
        , Staged.unstage (create_db_from_stanzas stanzas ~lib_config ~project) ))
    in
    let db_by_project_dir =
      Path.Build.Map.map make_db_by_project_dir ~f:(fun (project, make_db) ->
        project, make_db ~scopes ~owner:None ~public_libs ~instrument_with ~package_scopes)
    in
    let rocq_scopes =
      Rocq_scope.make
        context
        ~public_libs
        rocq_stanzas
        ~db_by_project_output_root:db_by_project_dir
    in
    let by_dir =
      Path.Build.Map.map db_by_project_dir ~f:(fun (loaded_project, db) ->
        let root = Loaded_project.output_root loaded_project in
        let project = Loaded_project.project loaded_project in
        let rocq_db = Rocq_scope.find rocq_scopes ~project:loaded_project in
        { loaded_project; project; db; public_libs; rocq_db; root })
    in
    let by_package =
      Path.Build.Map.to_list make_db_by_project_dir
      |> List.concat_map ~f:(fun (dir, (project, make_db)) ->
        let scope = Path.Build.Map.find_exn by_dir dir in
        Dune_project.including_hidden_packages (Loaded_project.project project)
        |> Package.Name.Map.to_list_map ~f:(fun name _ ->
          let public_libs = Package.Name.Map.find_exn public_libs_by_package name in
          let db =
            make_db
              ~scopes
              ~owner:(Some name)
              ~public_libs
              ~instrument_with
              ~package_scopes
          in
          name, { scope with db; public_libs }))
      |> Package.Name.Map.of_list_exn
    in
    { by_dir; by_package; dependency_libs_by_package }
  ;;

  let create ~context ~projects_by_root stanzas rocq_stanzas =
    let t = Fdecl.create Dyn.opaque in
    let* opaque_packages = Opam_package.all context in
    let package_scopes =
      lazy
        (Package.Name.Map.mapi opaque_packages ~f:(fun name package ->
           let scope = Package.Name.Map.find_exn (Fdecl.get t).by_package name in
           Path.build package.paths.target_dir, scope.db))
    in
    let* context = Context.DB.get context in
    let* lib_config =
      let+ ocaml = Context.ocaml context in
      ocaml.lib_config
    in
    let instrument_with = Context.instrument_with context in
    let+ make_public_libs =
      let+ installed_libs = Lib.DB.installed context in
      public_libs
        t
        ~context:(Context.name context)
        ~instrument_with
        ~installed_libs
        stanzas
      |> Staged.unstage
    in
    let public_libs = make_public_libs ~owner:None ~include_owner:true in
    let package_names =
      Path.Build.Map.values projects_by_root
      |> List.concat_map ~f:(fun project ->
        Loaded_project.project project
        |> Dune_project.including_hidden_packages
        |> Package.Name.Map.keys)
    in
    let public_libs_by_package =
      Package.Name.Map.of_list_map_exn package_names ~f:(fun name ->
        name, make_public_libs ~owner:(Some name) ~include_owner:true)
    in
    let dependency_libs_by_package =
      Package.Name.Map.of_list_map_exn package_names ~f:(fun name ->
        name, make_public_libs ~owner:(Some name) ~include_owner:false)
    in
    let value =
      scopes_by_dir
        ~scopes:t
        ~package_scopes
        ~lib_config
        ~projects_by_root
        ~public_libs
        ~public_libs_by_package
        ~dependency_libs_by_package
        ~instrument_with
        context
        stanzas
        rocq_stanzas
    in
    Fdecl.set t value;
    value, public_libs
  ;;

  let create_from_stanzas ~projects_by_root ~(context : Context_name.t) stanzas =
    let stanzas, rocq_stanzas =
      Dune_file.fold_static_stanzas
        stanzas
        ~init:([], [])
        ~f:(fun dune_file stanza (acc, rocq_acc) ->
          let loaded_dir = Dune_file.loaded_dir dune_file in
          match Stanza.repr stanza with
          | Library.T lib ->
            (loaded_dir, Library_related_stanza.Library lib) :: acc, rocq_acc
          | Deprecated_library_name.T d ->
            (loaded_dir, Deprecated_library_name d) :: acc, rocq_acc
          | Library_redirect.Local.T d ->
            (loaded_dir, Library_redirect d) :: acc, rocq_acc
          | Rocq_stanza.Theory.T rocq_lib ->
            ( acc
            , (Loaded_dir.project loaded_dir, Loaded_dir.output_dir loaded_dir, rocq_lib)
              :: rocq_acc )
          | _ -> acc, rocq_acc)
    in
    create ~projects_by_root ~context stanzas rocq_stanzas
  ;;

  let all =
    Per_context.create_by_name ~name:"scope" (fun context ->
      Memo.Lazy.create ~name:"scope" (fun () ->
        let* projects_by_root = Dune_load.projects_by_output_root context
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

  let find_by_project_root context project_root =
    let dir =
      match project_root with
      | None -> Context.build_dir context
      | Some project_root ->
        let build_context = Context.build_context context in
        Path.Build.append_source build_context.build_dir project_root
    in
    find_by_dir dir
  ;;

  let find_by_project context project =
    let+ scopes, _ = create_from_stanzas context in
    find_by_project scopes project
  ;;

  let for_package (scope : scope) = function
    | None -> Memo.return scope
    | Some package ->
      let* context = Context.DB.by_dir scope.root in
      let+ scopes, _ = create_from_stanzas (Context.name context) in
      Package.Name.Map.find_exn scopes.by_package (Package.name package)
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
      | Library lib -> Lib.Local.info lib |> Lib_info.loc
      | Deprecated_library_name { old_name = old_public_name, _; _ } ->
        Public_lib.loc old_public_name
    ;;

    module Set = struct
      type t =
        { libraries : Lib.Local.t list
        ; deprecated_library_names : Deprecated_library_name.t list
        }

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
    let package_entries public_libs stanzas =
      let+ libs =
        Memo.List.fold_left stanzas ~init:[] ~f:(fun acc (dune_file, stanza) ->
          match Stanza.repr stanza with
          | Library.T ({ enabled_if; _ } as lib) ->
            let src_dir = Dune_file.dir dune_file in
            let lib_dir = Dune_file.output_dir dune_file in
            let* db =
              match lib.visibility with
              | Public _ -> Memo.return public_libs
              | Private _ ->
                let+ scope = find_by_dir lib_dir in
                libs scope
            in
            let* enabled =
              let* expander = Expander0.get ~dir:lib_dir in
              Expander0.eval_blang expander enabled_if
            in
            if enabled
            then
              Lib.DB.find_lib_id db (Local (Library.to_lib_id ~src_dir lib))
              >>| function
              | None -> acc
              | Some lib -> Lib_entry.Library (Lib.Local.of_lib_exn lib) :: acc
            else Memo.return acc
          | Deprecated_library_name.T d ->
            Memo.return (Lib_entry.Deprecated_library_name d :: acc)
          | _ -> Memo.return acc)
      in
      Lib_entry.Set.of_list libs
    in
    let entries =
      Memo.create
        "package-library-entries"
        ~input:
          (module struct
            type t = Context_name.t * Package.Name.t

            let equal = Tuple.T2.equal Context_name.equal Package.Name.equal
            let hash = Tuple.T2.hash Context_name.hash Package.Name.hash
            let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn Package.Name.to_dyn
          end)
        (fun (ctx, pkg_name) ->
           let* public_libs = public_libs ctx
           and* stanzas = Dune_load.stanzas_of_package ctx pkg_name in
           package_entries public_libs stanzas)
    in
    fun (ctx : Context_name.t) pkg_name ->
      let+ libs = Memo.exec entries (ctx, pkg_name) in
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
              let compound =
                Compound_user_error.duplicate
                  ~main_loc:loc2
                  ~previous_loc:loc1
                  main_message
              in
              User_error.raise
                ~compound
                ~loc:loc2
                [ main_message
                ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
                ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
                ]))
      in
      libs
  ;;
end
