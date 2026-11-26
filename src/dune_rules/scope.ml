open Import
open Memo.O

type t =
  { loaded_project : Loaded_project.t
  ; db : Lib.DB.t
  ; public_libs : Lib.DB.t
  ; rocq_db : Rocq_lib.DB.t Memo.t
  }

let root t = Loaded_project.output_root t.loaded_project
let project t = Loaded_project.project t.loaded_project
let loaded_project t = t.loaded_project
let libs t = t.db
let public_libs t = t.public_libs
let rocq_libs t = t.rocq_db

let raise_duplicate_library ~show_location ~previous_loc ~main_loc text =
  let compound = Compound_user_error.duplicate ~main_loc ~previous_loc text in
  let loc = Option.some_if show_location main_loc in
  User_error.raise
    ?loc
    ~compound
    [ text
    ; Pp.textf "- %s" (Loc.to_file_colon_line previous_loc)
    ; Pp.textf "- %s" (Loc.to_file_colon_line main_loc)
    ]
;;

let resolve_executable_deps
      t
      ~db
      ({ Executables.buildable; forbidden_libraries; _ } as exes)
  =
  let dune_version = Dune_project.dune_version (project t) in
  let+ pps = Lib.DB.pps_for_preprocessing t.db buildable.preprocess.config in
  Lib.DB.resolve_user_written_deps
    db
    (Executables.exe_target exes)
    buildable.libraries
    ~allow_unused_libraries:buildable.allow_unused_libraries
    ~pps
    ~dune_version
    ~allow_overlaps:buildable.allow_overlapping_dependencies
    ~forbidden_libraries
;;

module DB = struct
  type scope = t

  (* The scopes of one partition of a context: the workspace, or one mounted
     package. *)
  type t =
    { by_dir : scope Path.Build.Map.t
    ; by_package : scope Package.Name.Map.t
    }

  type partition = t

  let find_by_dir t dir =
    let scope = Find_closest_source_dir.find_by_dir_exn t.by_dir ~dir in
    match Loaded_project.owner scope.loaded_project with
    | Some _ -> scope
    | None ->
      let src_dir = Dune_project.dir_in_context dir in
      (match Dune_project.exclusive_package (project scope) ~dir:src_dir with
       | None -> scope
       | Some package -> Package.Name.Map.find_exn t.by_package (Package.Id.name package))
  ;;

  let find_by_project_root t root = Path.Build.Map.find_exn t.by_dir root

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
    let resolve_found_or_redirect ~scopes ~root ~owner ~public_libs fr =
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
              | None -> find_by_project_root scopes root
              | Some package -> Package.Name.Map.find_exn scopes.by_package package
            in
            Lib.DB.Resolve_result.redirect_by_id scope.db (Lib_info.lib_id info))
        in
        Memo.return result
      | Deprecated_library_name lib ->
        Memo.return (Lib.DB.Resolve_result.redirect_by_name public_libs lib)
    in
    fun ~lib_config ~loaded_project stanzas ->
      let project = Loaded_project.project loaded_project in
      let root = Loaded_project.output_root loaded_project in
      let by_name, by_id, _ =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Map.empty, Lib_name.Map.empty)
          ~f:(fun (by_name, by_id, libname_conflict_map) (dune_file, stanza) ->
            let dir = Dune_file.output_dir dune_file in
            let src_dir = Dune_file.dir dune_file in
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
                  Library.to_lib_info conf ~expander ~dir ~lib_config |> Lib_info.of_local
                and lib_id = Library.to_lib_id ~src_dir conf in
                let owner =
                  match Loaded_project.owner loaded_project with
                  | Some _ as owner -> owner
                  | None ->
                    (match Library.package conf with
                     | Some package -> Some (Package.name package)
                     | None ->
                       Dune_project.exclusive_package project ~dir:src_dir
                       |> Option.map ~f:Package.Id.name)
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
                     raise_duplicate_library
                       ~show_location:false
                       ~main_loc:loc2
                       ~previous_loc:loc1
                       main_message))
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
          resolve_found_or_redirect ~scopes ~root ~owner ~public_libs
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
        { root : Path.Build.t
        ; lib_id : Lib_id.Local.t
        ; enabled : Toggle.t Memo.t
        ; loc : Loc.t
        }
    | Name of (Loc.t * Lib_name.t)

  (* The public libraries one partition defines, resolving into its scopes. *)
  module Public_libs = struct
    type t =
      { by_name : redirect_to list Lib_name.Map.t
      ; by_id : redirect_to Lib_id.Map.t
      ; scopes : partition Fdecl.t
      }

    let resolve_redirect_to t rt =
      match rt with
      | Project { root; lib_id; enabled; _ } ->
        let+ enabled =
          let+ toggle = enabled in
          Toggle.enabled toggle
        in
        if enabled
        then (
          let scope = find_by_project_root (Fdecl.get t.scopes) root in
          Lib.DB.Resolve_result.redirect_by_id scope.db (Local lib_id))
        else Lib.DB.Resolve_result.not_found
      | Name name -> Memo.return (Lib.DB.Resolve_result.redirect_in_the_same_db name)
    ;;

    let of_stanzas ~scopes stanzas =
      let by_name, by_id =
        List.fold_left
          stanzas
          ~init:(Lib_name.Map.empty, Lib_id.Map.empty)
          ~f:
            (fun
              (by_name, by_id)
              ((dune_file, stanza) : Dune_file.t * Library_related_stanza.t)
            ->
            let dir = Dune_file.output_dir dune_file in
            let candidate =
              match stanza with
              | Library ({ visibility = Public p; _ } as conf) ->
                let lib_id =
                  let src_dir = Dune_file.dir dune_file in
                  Library.to_lib_id ~src_dir conf
                in
                let enabled =
                  Memo.lazy_ ~name:"library-enabled" (fun () ->
                    let* expander = Expander0.get ~dir in
                    Expander0.eval_blang expander conf.enabled_if >>| Toggle.of_bool)
                  |> Memo.Lazy.force
                in
                let root =
                  Loaded_project.output_root (Dune_file.loaded_project dune_file)
                in
                Some
                  ( Public_lib.name p
                  , Project { root; lib_id; enabled; loc = Public_lib.loc p }
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
      { by_name; by_id; scopes }
    ;;
  end

  (* What every library database of a context shares. *)
  module Context_libs = struct
    type t =
      { context : Context.t
      ; installed_libs : Lib.DB.t
      ; lib_config : Lib_config.t
      ; instrument_with : Lib_name.t list
      }

    let get =
      Per_context.create_by_name ~name:"scope-context-libs" (fun context_name ->
        Memo.lazy_ ~name:"scope-context-libs" (fun () ->
          let* context = Context.DB.get context_name in
          let* lib_config =
            let+ ocaml = Context.ocaml context in
            ocaml.lib_config
          in
          let+ installed_libs =
            let* active = Pkg_rules.lock_dir_active context_name in
            if active
            then Lib.DB.of_paths context ~paths:[ lib_config.stdlib_dir ]
            else Lib.DB.installed context
          in
          { context
          ; installed_libs
          ; lib_config
          ; instrument_with = Context.instrument_with context
          })
        |> Memo.Lazy.force)
      |> Staged.unstage
    ;;
  end

  module Context_and_owner = struct
    type t = Context_name.t * Package.Name.t option

    let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn (Dyn.option Package.Name.to_dyn)
    let equal = Tuple.T2.equal Context_name.equal (Option.equal Package.Name.equal)
    let hash = Tuple.T2.hash Context_name.hash (Option.hash Package.Name.hash)
  end

  let scopes_by_dir
        ~scopes
        ~package_scopes
        ~lib_config
        ~projects_by_root
        ~owner_libs
        ~instrument_with
        context
        stanzas
        rocq_stanzas
    =
    let stanzas_by_project_dir =
      List.map stanzas ~f:(fun (dir, stanza) ->
        Loaded_project.output_root (Dune_file.loaded_project dir), (dir, stanza))
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
        ( loaded_project
        , Staged.unstage (create_db_from_stanzas stanzas ~lib_config ~loaded_project) ))
    in
    let scope_db make_db owner =
      let public_libs = owner_libs owner in
      public_libs, make_db ~scopes ~owner ~public_libs ~instrument_with ~package_scopes
    in
    let db_by_project_dir =
      Path.Build.Map.map make_db_by_project_dir ~f:(fun (project, make_db) ->
        let public_libs, db = scope_db make_db (Loaded_project.owner project) in
        project, public_libs, db)
    in
    let rocq_scopes =
      Rocq_scope.make
        context
        ~public_libs:(owner_libs None)
        rocq_stanzas
        ~db_by_project_dir:
          (Path.Build.Map.map db_by_project_dir ~f:(fun (project, _, db) -> project, db))
        ~projects_by_root
    in
    let by_dir =
      Path.Build.Map.mapi
        db_by_project_dir
        ~f:(fun dir (loaded_project, public_libs, db) ->
          let rocq_db = Rocq_scope.find rocq_scopes ~dir in
          { loaded_project; db; public_libs; rocq_db })
    in
    let by_package =
      Path.Build.Map.to_list make_db_by_project_dir
      |> List.concat_map ~f:(fun (dir, (project, make_db)) ->
        let scope = Path.Build.Map.find_exn by_dir dir in
        Loaded_project.packages project
        |> Package.Name.Map.to_list_map ~f:(fun name _ ->
          let public_libs, db = scope_db make_db (Some name) in
          name, { scope with db; public_libs }))
      |> Package.Name.Map.of_list_exn
    in
    { by_dir; by_package }
  ;;

  let library_related_stanzas dune_files =
    Dune_file.fold_static_stanzas
      dune_files
      ~init:([], [])
      ~f:(fun dune_file stanza (acc, rocq_acc) ->
        match Stanza.repr stanza with
        | Library.T lib ->
          (dune_file, Library_related_stanza.Library lib) :: acc, rocq_acc
        | Deprecated_library_name.T d ->
          (dune_file, Deprecated_library_name d) :: acc, rocq_acc
        | Library_redirect.Local.T d -> (dune_file, Library_redirect d) :: acc, rocq_acc
        | Rocq_stanza.Theory.T rocq_lib ->
          acc, (Dune_file.output_dir dune_file, rocq_lib) :: rocq_acc
        | _ -> acc, rocq_acc)
  ;;

  (* The scopes of a partition, with its public libraries: the workspace for
     [None], or the mounted package named. Loading a mount fetches its source,
     so only a lookup naming the package forces it. Each partition's databases
     reach the others on demand, by package name. *)
  let rec partition =
    lazy
      (Memo.create
         "scope-partition"
         ~input:(module Context_and_owner)
         (fun (context, owner) ->
            let* ctx = Context_libs.get context in
            let* loaded =
              match owner with
              | None -> Partitions.workspace context >>| Option.some
              | Some package -> Partitions.mount context package
            in
            match loaded with
            | None -> Memo.return None
            | Some loaded ->
              let+ candidates = Pkg_rules.candidates context in
              let scopes = Fdecl.create Dyn.opaque in
              let stanzas, rocq_stanzas =
                library_related_stanzas (Partitions.Partition.dune_files loaded)
              in
              let public = Public_libs.of_stanzas ~scopes stanzas in
              let package_scopes = package_scopes ctx candidates in
              let value =
                scopes_by_dir
                  ~scopes
                  ~package_scopes
                  ~lib_config:ctx.lib_config
                  ~projects_by_root:(Partitions.Partition.projects_by_output_root loaded)
                  ~owner_libs:(fun owner ->
                    owner_db ctx ~package_scopes ~owner ~include_owner:true)
                  ~instrument_with:ctx.instrument_with
                  ctx.context
                  stanzas
                  rocq_stanzas
              in
              Fdecl.set scopes value;
              Some (value, public)))

  and partition_exn context owner =
    Memo.exec (Lazy.force partition) (context, owner)
    >>| function
    | Some partition -> partition
    | None ->
      Code_error.raise
        "Scope.DB: no partition"
        [ "context", Context_name.to_dyn context
        ; "owner", Dyn.option Package.Name.to_dyn owner
        ]

  (* An opaque package reveals its libraries once it is installed, so looking
     in one builds it. *)
  and opaque_libs =
    lazy
      (Memo.create
         "opaque-package-libraries"
         ~input:(module Pkg_rules.Context_and_package)
         (fun (context, name) ->
            let* package = Pkg_rules.find_opaque context name in
            match package with
            | None -> Memo.return None
            | Some (_, dependency) ->
              let* dependency, _ = Action_builder.evaluate_and_collect_facts dependency in
              let lib_root = Pkg_rules.Dependency.lib_root dependency in
              let* findlib = Findlib.create_for_package context ~package:name ~lib_root in
              let* ctx = Context_libs.get context in
              let* ocaml = Context.ocaml ctx.context in
              let+ parent = package_db context name ~include_owner:false in
              let db =
                Lib.DB.create_from_findlib
                  findlib
                  ~has_bigarray_library:(Ocaml.Version.has_bigarray_library ocaml.version)
                  ~parent:(Some parent)
                  ~instrument_with:ctx.instrument_with
              in
              Some (findlib, db)))

  and package_db context name ~include_owner =
    let* ctx = Context_libs.get context in
    let+ candidates = Pkg_rules.candidates context in
    owner_db
      ctx
      ~package_scopes:(package_scopes ctx candidates)
      ~owner:(Some name)
      ~include_owner

  (* Transitive libraries installed by an opaque package are checked in that
     package's own database; see [Lib.DB.with_package_scopes]. *)
  and package_scopes ctx candidates =
    let rec package_scopes =
      lazy
        (Package.Name.Map.mapi candidates ~f:(fun name candidate ->
           let root =
             Pkg_rules.candidate_paths candidate
             |> Pkg_rules.Paths.target_dir
             |> Path.build
           in
           root, owner_db ctx ~package_scopes ~owner:(Some name) ~include_owner:true))
    in
    package_scopes

  (* Public dune libraries are independent of the requester. Opaque libraries
     are discovered by building their eligible package, in its own scope. *)
  and owner_db (ctx : Context_libs.t) ~package_scopes ~owner ~include_owner =
    let context = Context.name ctx.context in
    let opaque_packages =
      Memo.lazy_ ~name:"opaque-library-packages" (fun () ->
        match owner with
        | None -> Memo.return None
        | Some owner ->
          let+ packages = Partitions.package_closure_for_context context owner in
          Some
            (if include_owner then packages else Package.Name.Set.remove packages owner))
    in
    let eligible_opaque name =
      let+ packages = Memo.Lazy.force opaque_packages in
      match packages with
      | None -> true
      | Some packages -> Package.Name.Set.mem packages name
    in
    let resolve_in (public : Public_libs.t) name =
      Lib_name.Map.find public.by_name name
      |> Option.value ~default:[]
      |> Memo.List.map ~f:(Public_libs.resolve_redirect_to public)
    in
    let partition owner = partition_exn context owner >>| snd in
    let mounted_libs package =
      let* locked = Pkg_rules.find_locked context package in
      match locked with
      | None -> Memo.return `None
      | Some (_, `Opam) ->
        let+ eligible = eligible_opaque package in
        if eligible then `Opaque else `None
      | Some (_, `Dune _) ->
        let+ public = partition (Some package) in
        `Native public
    in
    let candidate_names () = Pkg_rules.dependencies context >>| Package.Name.Map.keys in
    (* Scanning a package builds it. One that needs the workspace is only
       scanned for a requester that declares it and that it does not need. *)
    let opaque names =
      let* names = Memo.List.filter names ~f:eligible_opaque in
      if List.is_empty names
      then Memo.return []
      else
        let* reverse = Pkg_rules.workspace_dependencies context in
        List.filter names ~f:(fun name ->
          match Package.Name.Map.find reverse name with
          | None -> true
          | Some reached ->
            (match owner with
             | Some owner -> not (Package.Name.Set.mem reached owner)
             | None -> false))
        |> Memo.List.filter ~f:(fun name ->
          Pkg_rules.find_locked context name
          >>| function
          | Some (_, `Opam) -> true
          | Some (_, `Dune _) | None -> false)
    in
    let search name packages =
      Memo.parallel_map packages ~f:(fun package ->
        let* libraries = Memo.exec (Lazy.force opaque_libs) (context, package) in
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
    let resolve name =
      let* results = partition None >>= fun public -> resolve_in public name in
      if List.is_non_empty results
      then Memo.return results
      else (
        (* The locked package named like a library. A dune package defines
           only libraries named after it. *)
        let package = Lib_name.package_name name in
        let* results =
          mounted_libs package
          >>= function
          | `None -> Memo.return []
          | `Native public -> resolve_in public name
          | `Opaque -> search name [ package ]
        in
        if List.is_non_empty results
        then Memo.return results
        else
          let* installed = Lib.DB.available ctx.installed_libs name in
          if installed
          then Memo.return []
          else
            let* candidates = candidate_names () in
            opaque candidates >>= search name)
    in
    let resolve_lib_id (lib_id : Lib_id.t) =
      match lib_id with
      | External (loc, name) ->
        Memo.return (Lib.DB.Resolve_result.redirect_in_the_same_db (loc, name))
      | Local id ->
        (* A local id names a library by its local name and its directory,
           which says which partition defines it. *)
        let* public =
          match Pkg_rules.package_of_dir_in_context (Lib_id.Local.src_dir id) with
          | None -> partition None >>| Option.some
          | Some package ->
            mounted_libs package
            >>| (function
             | `Native public -> Some public
             | `None | `Opaque -> None)
        in
        (match public with
         | None -> Memo.return Lib.DB.Resolve_result.not_found
         | Some public ->
           (match Lib_id.Map.find public.by_id lib_id with
            | None -> Memo.return Lib.DB.Resolve_result.not_found
            | Some rt -> Public_libs.resolve_redirect_to public rt))
    in
    let all () =
      let* workspace =
        partition None >>| fun public -> Lib_name.Map.keys public.by_name
      in
      let+ locked =
        candidate_names ()
        >>= Memo.List.concat_map ~f:(fun name ->
          mounted_libs name
          >>= function
          | `None -> Memo.return []
          | `Native public -> Memo.return (Lib_name.Map.keys public.by_name)
          | `Opaque ->
            Memo.exec (Lazy.force opaque_libs) (context, name)
            >>= (function
             | None -> Memo.return []
             | Some (findlib, _) ->
               Findlib.all_packages findlib >>| List.map ~f:Dune_package.Entry.name))
      in
      List.sort_uniq (workspace @ locked) ~compare:Lib_name.compare
    in
    Lib.DB.create
      ()
      ~parent:(Some ctx.installed_libs)
      ~resolve
      ~resolve_lib_id
      ~all
      ~instrument_with:ctx.instrument_with
    |> Lib.DB.with_package_scopes ~package_scopes
  ;;

  let partition context owner = Memo.exec (Lazy.force partition) (context, owner)
  let workspace context = partition_exn context None >>| fst

  (* The partition holding a build directory: a mount with projects, or the
     workspace. *)
  let of_dir context dir =
    match Pkg_rules.package_of_artifact_path dir with
    | None -> workspace context
    | Some package ->
      partition context (Some package)
      >>= (function
       | Some (scopes, _) when not (Path.Build.Map.is_empty scopes.by_dir) ->
         Memo.return scopes
       | Some _ | None -> workspace context)
  ;;

  let with_all context ~f =
    let+ scopes = workspace (Context.name context) in
    let find project =
      Path.Build.append_local (Context.build_dir context) (Dune_project.root project)
      |> find_by_project_root scopes
    in
    f find
  ;;

  let public_libs context =
    let+ scopes = workspace context in
    let root = Context_name.build_dir context in
    (Find_closest_source_dir.find_by_dir_exn scopes.by_dir ~dir:root).public_libs
  ;;

  let find_by_dir dir =
    let* context = Context.DB.by_dir dir in
    let+ scopes = of_dir (Context.name context) dir in
    find_by_dir scopes dir
  ;;

  let find_by_project context project =
    let root =
      Path.Build.append_local (Context_name.build_dir context) (Dune_project.root project)
    in
    let+ scopes = of_dir context root in
    find_by_project_root scopes root
  ;;

  let find_by_project_root context project_root =
    let dir =
      match project_root with
      | None -> Context.build_dir context
      | Some project_root ->
        let build_context = Context.build_context context in
        Path.Build.append_local build_context.build_dir project_root
    in
    find_by_dir dir
  ;;

  let for_package (scope : scope) = function
    | None -> Memo.return scope
    | Some _ when Option.is_some (Loaded_project.owner scope.loaded_project) ->
      Memo.return scope
    | Some package ->
      let* context = Context.DB.by_dir (root scope) in
      let+ scopes = workspace (Context.name context) in
      Package.Name.Map.find_exn scopes.by_package (Package.name package)
  ;;

  let libs_for_package context name =
    let* package = Pkg_rules.find_opaque context name in
    match package with
    | None -> Memo.return None
    | Some _ -> package_db context name ~include_owner:true >>| Option.some
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
        ~input:(module Pkg_rules.Context_and_package)
        (fun (ctx, pkg_name) ->
           let* public_libs = public_libs ctx
           and* stanzas = Partitions.stanzas_of_package ctx pkg_name in
           package_entries public_libs stanzas)
    in
    fun (ctx : Context_name.t) pkg_name ->
      let+ libs = Memo.exec entries (ctx, pkg_name) in
      match libs with
      | { libraries = []; deprecated_library_names = [] } -> libs
      | libs ->
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
                raise_duplicate_library
                  ~show_location:true
                  ~main_loc:loc2
                  ~previous_loc:loc1
                  main_message))
        in
        libs
  ;;
end
