open Import
open Memo.O

include struct
  open Dune_pkg
  module Package_variable = Package_variable
  module Substs = Substs
  module Checksum = Checksum
  module Source = Source
  module Build_command = Lock_dir.Build_command
  module Pkg_info = Lock_dir.Pkg_info
  module Depexts = Lock_dir.Depexts
  module Digest_feed = Dune_digest.Feed
  module Dune_dep = Dune_dep
end

module Package_universe = struct
  (* A type of group of packages that are co-installed. Multiple different
     versions of a package may be co-installed into the same universe.

     Note that a dev tool universe just contains the package for the dev tool
     itself and not its dependencies, which are installed into the
     [Dependencies _] universe for the default context so they may be
     shared with the project's dependencies. *)
  type t =
    | Dependencies of Context_name.t
    | Dev_tool of Dune_pkg.Dev_tool.t

  let equal a b =
    match a, b with
    | Dependencies a, Dependencies b -> Context_name.equal a b
    | Dev_tool a, Dev_tool b -> Dune_pkg.Dev_tool.equal a b
    | _ -> false
  ;;

  let hash t =
    match t with
    | Dependencies context_name ->
      Tuple.T2.hash Int.hash Context_name.hash (0, context_name)
    | Dev_tool dev_tool -> Tuple.T2.hash Int.hash Dune_pkg.Dev_tool.hash (1, dev_tool)
  ;;

  let context_name = function
    | Dependencies context_name -> context_name
    | Dev_tool _ ->
      (* Dev tools can only be built in the default context. *)
      Context_name.default
  ;;

  let lock_dir_path t =
    match t with
    | Dependencies ctx -> Lock_dir.get_path ctx
    | Dev_tool dev_tool ->
      Lock_dir.dev_tool_lock_dir dev_tool |> Option.some |> Memo.return
  ;;

  let lock_dir t =
    match t with
    | Dependencies ctx -> Lock_dir.get_exn ctx
    | Dev_tool dev_tool -> Lock_dir.of_dev_tool dev_tool
  ;;
end

module Pkg_digest = struct
  module T = struct
    type t =
      { name : Package.Name.t
      ; version : Package_version.t
      ; lockfile_and_dependency_digest : Dune_digest.t
        (* A hash of the package's lockfile as well as of the digests of all
           the package's dependencies. *)
      }

    let equal { name; version; lockfile_and_dependency_digest } t =
      Package.Name.equal name t.name
      && Package_version.equal version t.version
      && Dune_digest.equal lockfile_and_dependency_digest t.lockfile_and_dependency_digest
    ;;

    let compare { name; version; lockfile_and_dependency_digest } t =
      let open Ordering.O in
      let= () = Package.Name.compare name t.name in
      let= () = Package_version.compare version t.version in
      Dune_digest.compare lockfile_and_dependency_digest t.lockfile_and_dependency_digest
    ;;

    let to_dyn { name; version; lockfile_and_dependency_digest } =
      Dyn.record
        [ "name", Package.Name.to_dyn name
        ; "version", Package_version.to_dyn version
        ; ( "lockfile_and_dependency_digest"
          , Dune_digest.to_dyn lockfile_and_dependency_digest )
        ]
    ;;

    let hash { name; version; lockfile_and_dependency_digest } =
      Tuple.T3.hash
        Package.Name.hash
        Package_version.hash
        Dune_digest.hash
        (name, version, lockfile_and_dependency_digest)
    ;;

    let digest_repr = Repr.view Repr.string ~to_:Dune_digest.to_string

    let repr =
      Repr.record
        "pkg-digest"
        [ Repr.field "name" Package.Name.repr ~get:(fun t -> t.name)
        ; Repr.field "version" Package_version.repr ~get:(fun t -> t.version)
        ; Repr.field "lockfile_and_dependency_digest" digest_repr ~get:(fun t ->
            t.lockfile_and_dependency_digest)
        ]
    ;;
  end

  include T
  include Comparable.Make (T)

  let to_string { name; version; lockfile_and_dependency_digest } =
    sprintf
      "%s.%s-%s"
      (Package.Name.to_string name)
      (Package_version.to_string version)
      (Dune_digest.to_string lockfile_and_dependency_digest)
  ;;

  let of_string s =
    let parse_error msg =
      User_error.raise [ Pp.textf "Failed to parse %S as a package pkg digest." s; msg ]
    in
    match String.lsplit2 s ~on:'.' with
    | None -> parse_error (Pp.text "Missing '.' between name and version.")
    | Some (name, rest) ->
      (match String.rsplit2 rest ~on:'-' with
       | None -> parse_error (Pp.text "Missing '-' between version and lockfile digest.")
       | Some (version, lockfile_and_dependency_digest) ->
         (match Dune_digest.from_hex lockfile_and_dependency_digest with
          | None ->
            parse_error
              (Pp.textf "Failed to parse %S as digest" lockfile_and_dependency_digest)
          | Some lockfile_and_dependency_digest ->
            let name = Package.Name.of_string name in
            let version = Package_version.of_string version in
            { name; version; lockfile_and_dependency_digest }))
  ;;

  let create lockfile_pkg depends_pkg_digests =
    let lockfile_and_dependency_digest =
      Digest_feed.compute_digest
        (Digest_feed.tuple2 Lock_dir.Pkg.digest_feed (Digest_feed.repr (Repr.list repr)))
        (Dune_pkg.Lock_dir.Pkg.remove_locs lockfile_pkg, depends_pkg_digests)
    in
    let name = lockfile_pkg.info.name in
    let version = lockfile_pkg.info.version in
    { name; version; lockfile_and_dependency_digest }
  ;;
end

module Paths = struct
  include Opam_build.Paths

  let make pkg_digest universe =
    let root =
      match (universe : Package_universe.t) with
      | Dependencies ctx ->
        Path.Build.L.relative
          Private_context.t.build_dir
          [ Context_name.to_string ctx; ".pkg"; Pkg_digest.to_string pkg_digest ]
      | Dev_tool dev_tool -> Pkg_dev_tool.universe_install_path dev_tool
    in
    of_root pkg_digest.name ~root
  ;;
end

module Variable = Opam_build.Variable
module Install_cookie = Opam_build.Install_cookie
module Value_list_env = Opam_build.Value_list_env
module Env_update = Opam_build.Env_update

module Pkg = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; context : Context_name.t
    ; native : bool
    ; recipe : Opam_build.Recipe.t
    ; depends : t list
    ; depends_on_dune : bool
      (* whether the package declares a dependency on Dune, even if Dune is stripped from [depends] *)
    ; info : Pkg_info.t
    ; paths : Path.t Paths.t
    ; write_paths : Path.Build.t Paths.t
    ; files_dir : Path.Build.t
    ; mounted : Pkg_sources.Candidate.t option
    ; pkg_digest : Pkg_digest.t
    ; mutable exported_env : string Env_update.t list
    }

  module Top_closure = Top_closure.Make (Id.Set) (Monad.Id)

  let top_closure depends =
    match
      Top_closure.top_closure depends ~key:(fun t -> t.id) ~deps:(fun t -> t.depends)
    with
    | Ok s -> s
    | Error cycle ->
      User_error.raise
        [ Pp.text "the following packages form a cycle:"
        ; Pp.chain cycle ~f:(fun pkg ->
            Pp.verbatim (Package.Name.to_string pkg.info.name))
        ]
  ;;

  let deps_closure t = top_closure t.depends

  let source_files t ~loc =
    let skip_dir = function
      | ".hg" | ".git" | "_darcs" | "_opam" | "_build" | "_esy" -> true
      | _ -> false
    in
    let skip_file = String.starts_with ~prefix:".#" in
    let rec loop root acc path =
      let full_path = Path.Outside_build_dir.append_local root path in
      Fs_memo.dir_contents full_path
      >>= function
      | Error e ->
        User_error.raise
          ~loc
          [ Pp.textf
              "Unable to read %s"
              (Path.Outside_build_dir.to_string_maybe_quoted full_path)
          ; Unix_error.Detailed.pp e
          ]
      | Ok contents ->
        let files, dirs =
          let contents = Fs_memo.Dir_contents.to_list contents in
          List.rev_filter_partition_map contents ~f:(fun (name, kind) ->
            (* TODO handle links and cycles correctly *)
            let name_s = Filename.to_string name in
            let relative = Path.Local.relative_fname path name in
            match kind with
            | S_DIR -> if skip_dir name_s then Skip else Right relative
            | _ -> if skip_file name_s then Skip else Left relative)
        in
        let acc = Path.Local.Set.of_list files |> Path.Local.Set.union acc in
        Memo.map_reduce
          dirs
          ~f:(fun dir -> loop root Path.Local.Set.empty dir)
          ~empty:acc
          ~combine:Path.Local.Set.union
    in
    (match t.info.source with
     | None -> Memo.return None
     | Some source ->
       Lock_dir.source_kind source
       >>| (function
        | `Local (`File, _) | `Fetch -> None
        | `Local (`Directory, root) -> Some root))
    >>= function
    | None -> Memo.return Path.Local.Set.empty
    | Some root -> loop root Path.Local.Set.empty Path.Local.root
  ;;

  let dependency t =
    if not t.native
    then
      Opam_build.Dependency.load
        t.paths
        ~variables:(Pkg_info.variables t.info)
        ~exported_env:t.recipe.exported_env
    else
      let open Action_builder.O in
      let+ layout =
        Install_layout.materialize
          t.context
          [ { Opam_build.Dependency.paths = t.paths
            ; files = Section.Map.empty
            ; variables = Pkg_info.variables t.info
            ; exported_env = t.recipe.exported_env
            ; export_paths = t.paths
            }
          ]
      in
      Install_layout.dependencies layout |> List.hd
  ;;

  let package_deps t =
    Action_builder.List.iter (deps_closure t) ~f:(fun pkg ->
      Action_builder.ignore (dependency pkg))
  ;;

  let install_roots t =
    let default_install_roots = Paths.install_roots t.paths in
    match Pkg_toolchain.is_compiler_package_with_toolchains_enabled t.info.name with
    | false -> default_install_roots
    | true ->
      (* Compiler packages store their libraries in a subdirectory named "ocaml". *)
      { default_install_roots with
        lib_root = Path.relative default_install_roots.lib_root "ocaml"
      }
  ;;

  (* Given a list of packages, construct an env containing variables
     set by each package. Variables containing delimited lists of
     paths (e.g. PATH) which appear in multiple package's envs are
     concatenated in the reverse order of their associated packages in
     the input list. Environment updates via the `exported_env` field
     (equivalent to opam's `setenv` field) are applied for each
     package in the same order as the argument list. *)
  let build_env_of_deps ts =
    List.fold_left ts ~init:Env.Map.empty ~f:(fun env t ->
      let env =
        let roots = install_roots t in
        let init = Value_list_env.add_path env Env_path.var roots.bin in
        let vars = Install.Roots.to_env_without_path roots ~relative:Path.relative in
        List.fold_left vars ~init ~f:(fun acc (var, path) ->
          Value_list_env.add_path acc var path)
      in
      List.fold_left t.exported_env ~init:env ~f:Env_update.set)
  ;;

  (* [build_env t] returns an env containing paths containing all the
     tools and libraries required to build the package [t] inside the
     faux opam directory contained in the _build dir. *)
  let build_env t = build_env_of_deps @@ deps_closure t

  let base_env t =
    Env.Map.of_list_exn
      [ Opam_switch.opam_switch_prefix_var_name, [ Value.Path t.paths.target_dir ]
      ; Env.Var.of_string "CDPATH", [ Value.String "" ]
      ; Env.Var.of_string "MAKELEVEL", [ Value.String "" ]
      ; ( Env.Var.of_string "OPAM_PACKAGE_NAME"
        , [ Value.String (Package.Name.to_string t.info.name) ] )
      ; ( Env.Var.of_string "OPAM_PACKAGE_VERSION"
        , [ Value.String (Package_version.to_string t.info.version) ] )
      ; Env.Var.of_string "OPAMCLI", [ Value.String "2.0" ]
      ; Env.Var.of_string "OPAMSWITCH", [ Value.String "dune" ]
      ]
  ;;

  (* [exported_value_env t] returns the complete env that will be used
     to build the package [t] *)
  let exported_value_env t =
    let package_env = build_env t |> Env.Map.superpose (base_env t) in
    (* TODO: Run actions in a constrained environment. [Global.env ()] is the
       environment from which dune was executed, and some of the environment
       variables may affect builds in unintended ways and make builds less
       reproducible. However other environment variables must be set in order
       for build actions to run successfully, such as $PATH on systems where the
       shell's default $PATH variable doesn't include the location of standard
       programs or build tools (e.g. NixOS). *)
    Value_list_env.extend_concat_path (Lazy.force Value_list_env.global) package_env
  ;;

  let exported_env t = Value_list_env.to_env @@ exported_value_env t
end

module Pkg_installed = struct
  type t = { cookie : Install_cookie.t Action_builder.t }

  let of_paths (paths : Path.t Paths.t) =
    let cookie =
      let open Action_builder.O in
      let path = Paths.install_cookie paths in
      let+ () = path |> Dep.file |> Action_builder.dep in
      Install_cookie.load_exn path
    in
    { cookie }
  ;;
end

module Action_expander = struct
  include Opam_build.Action_expander
  module Expander = Opam_build.Action_expander.Expander

  module Artifacts_and_deps = struct
    type artifacts_and_deps =
      { binaries : Path.t Filename.Map.t
      ; dep_info :
          (OpamVariable.variable_contents Package_variable_name.Map.t * Path.t Paths.t)
            Package.Name.Map.t
      }

    let empty = { binaries = Filename.Map.empty; dep_info = Package.Name.Map.empty }

    let of_closure closure =
      Memo.parallel_map closure ~f:(fun (pkg : Pkg.t) ->
        Action_builder.evaluate_and_collect_facts (Pkg.dependency pkg) >>| fst)
      |> Memo.map ~f:(fun dependencies ->
        List.fold_left
          dependencies
          ~init:empty
          ~f:(fun { binaries; dep_info } (dependency : Opam_build.Dependency.t) ->
            let binaries =
              Section.Map.Multi.find dependency.files Bin
              |> List.fold_left ~init:binaries ~f:(fun acc bin ->
                Filename.Map.set
                  acc
                  (Filename.of_string_exn
                     (Bin.strip_exe (Path.basename bin |> Filename.to_string)))
                  bin)
            in
            let dep_info =
              Package.Name.Map.add_exn
                dep_info
                dependency.paths.name
                (dependency.variables, dependency.paths)
            in
            { binaries; dep_info }))
    ;;
  end

  let expander context (pkg : Pkg.t) =
    let closure =
      Memo.lazy_
        ~name:"package-dependency-closure"
        ~human_readable_description:(fun () ->
          Pp.textf
            "Computing closure for package %S"
            (Package.Name.to_string pkg.info.name))
        (fun () -> Pkg.deps_closure pkg |> Artifacts_and_deps.of_closure)
    in
    let env = Pkg.exported_value_env pkg in
    let depends =
      Memo.Lazy.map
        ~name:"package-dependency-info"
        closure
        ~f:(fun { Artifacts_and_deps.dep_info; _ } ->
          Package.Name.Map.add_exn
            dep_info
            pkg.info.name
            (Pkg_info.variables pkg.info, pkg.paths))
      |> Memo.Lazy.force
    in
    let artifacts =
      let+ { Artifacts_and_deps.binaries; _ } = Memo.Lazy.force closure in
      binaries
    in
    { Expander.paths = pkg.paths
    ; name = pkg.info.name
    ; artifacts
    ; context
    ; path = Env_path.path (Global.env ())
    ; depends
    ; files = Memo.return Package.Name.Map.empty
    ; depexts = pkg.recipe.depexts
    ; version = pkg.info.version
    ; env
    }
  ;;
end

module DB = struct
  let default_system_provided = Package.Name.Set.singleton Dune_pkg.Dune_dep.name

  module Pkg_table = struct
    module Pkg = Lock_dir.Pkg

    type dep =
      { dep_pkg : Pkg.t
      ; dep_loc : Loc.t
      ; dep_pkg_digest : Pkg_digest.t
      }

    type entry =
      { pkg : Pkg.t
      ; deps : dep list
      ; has_dune_dep : bool
      ; pkg_digest : Pkg_digest.t
      }

    let entries_by_name_of_lock_dir
          (lock_dir : Dune_pkg.Lock_dir.t)
          ~platform
          ~system_provided
      =
      let pkgs_by_name = Dune_pkg.Lock_dir.packages_on_platform lock_dir ~platform in
      let cache =
        (* Cache so that the digest of each package is only computed once *)
        Package.Name.Table.create 10
      in
      let rec compute_entry (pkg : Pkg.t) ~seen_set ~seen_list =
        if Package.Name.Set.mem seen_set pkg.info.name
        then
          User_error.raise
            [ Pp.textf "Dependency cycle between packages:"
            ; Pp.chain
                (List.rev (pkg :: seen_list))
                ~f:(fun (pkg : Pkg.t) ->
                  Pp.textf
                    "%s.%s"
                    (Package.Name.to_string pkg.info.name)
                    (Package_version.to_string pkg.info.version))
            ];
        Package.Name.Table.find_or_add cache pkg.info.name ~f:(fun name ->
          let seen_set = Package.Name.Set.add seen_set name in
          let seen_list = pkg :: seen_list in
          let has_dune_dep, deps =
            Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform pkg.depends ~platform
            |> Option.value ~default:[]
            |> List.fold_right
                 ~init:(false, [])
                 ~f:
                   (fun
                     { Dune_pkg.Lock_dir.Dependency.name; loc = dep_loc }
                     (has_dune_dep, acc)
                   ->
                   match
                     ( Dune_lang.Package_name.equal name Dune_dep.name
                     , Package.Name.Set.mem system_provided name )
                   with
                   | true, _ -> true, acc
                   | false, true -> has_dune_dep, acc
                   | _, false ->
                     let dep_pkg = Package.Name.Map.find_exn pkgs_by_name name in
                     let dep_entry = compute_entry dep_pkg ~seen_set ~seen_list in
                     ( has_dune_dep
                     , { dep_pkg; dep_loc; dep_pkg_digest = dep_entry.pkg_digest } :: acc
                     ))
          in
          let pkg_digest =
            Pkg_digest.create
              pkg
              (List.map deps ~f:(fun { dep_pkg_digest; _ } -> dep_pkg_digest))
          in
          { pkg; deps; has_dune_dep; pkg_digest })
      in
      Package.Name.Map.map
        pkgs_by_name
        ~f:(compute_entry ~seen_set:Package.Name.Set.empty ~seen_list:[])
    ;;

    (* Associate each package's digest with the package and its dependencies. *)
    type t = entry Pkg_digest.Map.t

    let of_lock_dir lock_dir ~platform ~system_provided =
      entries_by_name_of_lock_dir lock_dir ~platform ~system_provided
      |> Package.Name.Map.values
      |> Pkg_digest.Map.of_list_map_exn ~f:(fun entry -> entry.pkg_digest, entry)
    ;;

    (* Helper which is called when both tables have an entry with the same
       digest. This happens when two lock directories have a package in common
       and the transitive dependency closure of the package is identical in both
       lock directories. Here we assert that the packages and their immediate
       dependencies are identical as a sanity check. *)
    let union_check
          pkg_digest
          ({ pkg = pkg_a; deps = deps_a; has_dune_dep = _; pkg_digest = _ } as entry)
          { pkg = pkg_b; deps = deps_b; has_dune_dep = _; pkg_digest = _ }
      =
      if not (Pkg.equal (Pkg.remove_locs pkg_a) (Pkg.remove_locs pkg_b))
      then
        Code_error.raise
          "Two packages with the same pkg digest differ in their fields"
          [ "pkg_digest", Pkg_digest.to_dyn pkg_digest
          ; "pkg_a", Pkg.to_dyn pkg_a
          ; "pkg_b", Pkg.to_dyn pkg_b
          ];
      List.combine deps_a deps_b
      |> List.iter ~f:(fun (dep_a, dep_b) ->
        if not (Pkg.equal (Pkg.remove_locs dep_a.dep_pkg) (Pkg.remove_locs dep_b.dep_pkg))
        then
          Code_error.raise
            "Two packages with the same pkg digest differ in their dependencies"
            [ "pkg_digest", Pkg_digest.to_dyn pkg_digest
            ; "pkg_a", Pkg.to_dyn pkg_a
            ; "pkg_b", Pkg.to_dyn pkg_b
            ; "dep_of_a", Pkg.to_dyn dep_a.dep_pkg
            ; "dep_of_b", Pkg.to_dyn dep_b.dep_pkg
            ]);
      Some entry
    ;;

    let empty = Pkg_digest.Map.empty
    let union = Pkg_digest.Map.union ~f:union_check
    let union_all = Pkg_digest.Map.union_all ~f:union_check

    let of_dev_tool_deps_if_lock_dir_exists dev_tool ~platform ~system_provided =
      let+ lock_dir_opt = Lock_dir.of_dev_tool_if_lock_dir_exists dev_tool in
      Option.map lock_dir_opt ~f:(of_lock_dir ~platform ~system_provided)
    ;;

    let all_existing_dev_tools =
      Memo.lazy_ ~name:"all-existing-dev-tools" (fun () ->
        let* platform = Lock_dir.Sys_vars.solver_env in
        let+ xs =
          Memo.List.map
            Pkg_dev_tool.all
            ~f:
              (of_dev_tool_deps_if_lock_dir_exists
                 ~platform
                 ~system_provided:default_system_provided)
        in
        List.filter_opt xs |> union_all)
    ;;
  end

  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; pkg_digest_table : Pkg_table.t
    ; system_provided : Package.Name.Set.t
    }

  let equal x y = Id.equal x.id y.id

  let create ~pkg_digest_table ~system_provided =
    { id = Id.gen (); pkg_digest_table; system_provided }
  ;;

  let pkg_digest_of_name lock_dir platform pkg_name ~system_provided =
    let entries_by_name =
      Pkg_table.entries_by_name_of_lock_dir lock_dir ~platform ~system_provided
    in
    let entry = Package.Name.Map.find_exn entries_by_name pkg_name in
    entry.pkg_digest
  ;;

  let of_ctx =
    let of_ctx_memo =
      Memo.create
        "pkg-db"
        ~input:
          (module struct
            type t = Context_name.t * bool

            let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn Dyn.bool
            let hash = Tuple.T2.hash Context_name.hash Bool.hash
            let equal = Tuple.T2.equal Context_name.equal Bool.equal
          end)
        (fun (ctx, allow_sharing) ->
           Per_context.valid ctx
           >>= function
           | false ->
             Code_error.raise "invalid context" [ "context", Context_name.to_dyn ctx ]
           | true ->
             (* Dev tools are built in the default context, so allow their
                dependencies to be shared with the project's if it too is being
                built in the default context. *)
             let allow_sharing = allow_sharing && Context_name.is_default ctx in
             (* Is this value anything other than [default_system_provided]? *)
             let system_provided = default_system_provided in
             let+ pkg_digest_table =
               let* lock_dir = Lock_dir.get_exn ctx
               and* platform = Lock_dir.Sys_vars.solver_env in
               (if allow_sharing
                then Memo.Lazy.force Pkg_table.all_existing_dev_tools
                else Memo.return Pkg_table.empty)
               >>| Pkg_table.union
                     (Pkg_table.of_lock_dir lock_dir ~platform ~system_provided)
             in
             create ~pkg_digest_table ~system_provided)
    in
    fun ctx ~allow_sharing -> Memo.exec of_ctx_memo (ctx, allow_sharing)
  ;;

  (* Returns the db for the given context and the digest of the given package
     within that context. *)
  let of_project_pkg ctx pkg_name =
    let* lock_dir = Lock_dir.get_exn ctx
    and* platform = Lock_dir.Sys_vars.solver_env in
    let+ t = of_ctx ctx ~allow_sharing:true in
    t, pkg_digest_of_name lock_dir platform pkg_name ~system_provided:t.system_provided
  ;;

  (* Returns the db for all dev tools combined with the default context, and
     the digest for the dev tool's package. *)
  let of_dev_tool =
    let system_provided = default_system_provided in
    let inactive_lockdir =
      Memo.lazy_ ~name:"inactive-lockdir-package-db" (fun () ->
        let+ pkg_digest_table = Memo.Lazy.force Pkg_table.all_existing_dev_tools in
        create ~pkg_digest_table ~system_provided)
    in
    let of_dev_tool_memo =
      Memo.create "pkg-db-dev-tool" ~input:(module Dune_pkg.Dev_tool)
      @@ fun dev_tool ->
      let+ lock_dir = Lock_dir.of_dev_tool dev_tool
      and+ platform = Lock_dir.Sys_vars.solver_env in
      pkg_digest_of_name
        lock_dir
        platform
        (Pkg_dev_tool.package_name dev_tool)
        ~system_provided
    in
    fun dev_tool ->
      let+ db =
        Lock_dir.lock_dir_active Context_name.default
        >>= function
        | false -> Memo.Lazy.force inactive_lockdir
        | true -> of_ctx Context_name.default ~allow_sharing:true
      and+ pkg_digest = Memo.exec of_dev_tool_memo dev_tool in
      db, pkg_digest
  ;;
end

module rec Resolve : sig
  val resolve : DB.t -> Loc.t -> Pkg_digest.t -> Package_universe.t -> Pkg.t Memo.t
end = struct
  open Resolve

  module Input = struct
    type t =
      { db : DB.t
      ; pkg_digest : Pkg_digest.t
      ; universe : Package_universe.t
      }

    let equal { db; pkg_digest; universe } t =
      Pkg_digest.equal pkg_digest t.pkg_digest
      && Package_universe.equal universe t.universe
      && DB.equal db t.db
    ;;

    let hash { db = _; pkg_digest; universe } =
      Tuple.T2.hash Pkg_digest.hash Package_universe.hash (pkg_digest, universe)
    ;;

    let to_dyn = Dyn.opaque
  end

  let relocate action =
    let string_with_vars =
      String_with_vars.map_loc ~f:Dune_pkg.Lock_dir.loc_in_source_tree
    in
    let slang = Slang.map_loc ~f:Dune_pkg.Lock_dir.loc_in_source_tree in
    let blang = Slang.Blang.map_loc ~f:Dune_pkg.Lock_dir.loc_in_source_tree in
    Dune_lang.Action.map action ~string_with_vars ~slang ~blang
  ;;

  let relocate_build b =
    match (b : Build_command.t) with
    | Dune -> Build_command.Dune
    | Action a -> Build_command.Action (relocate a)
  ;;

  let is_relocatable_compiler_marker name =
    let relocatable_compiler = Package.Name.of_string "relocatable-compiler" in
    let relocatable = Package.Name.of_string "relocatable" in
    Package.Name.equal name relocatable_compiler || Package.Name.equal name relocatable
  ;;

  let has_relocatable_compiler_marker (info : Pkg_info.t) depends =
    is_relocatable_compiler_marker info.name
    || Pkg.top_closure depends
       |> List.exists ~f:(fun (pkg : Pkg.t) ->
         is_relocatable_compiler_marker pkg.info.name)
  ;;

  let is_compiler_version_relocatable (info : Pkg_info.t) =
    Pkg_toolchain.is_compiler_package_with_toolchains_enabled info.name
    &&
    match Package_version.compare info.version (Package_version.of_string "5.5.0") with
    | Lt -> false
    | Eq | Gt -> true
  ;;

  let resolve_impl { Input.db; pkg_digest; universe = package_universe } =
    match Pkg_digest.Map.find db.pkg_digest_table pkg_digest with
    | None -> Memo.return None
    | Some
        { pkg =
            { Lock_dir.Pkg.build_command
            ; install_command
            ; depends = _
            ; info
            ; exported_env
            ; depexts
            ; enabled_on_platforms = _
            } as pkg
        ; deps
        ; has_dune_dep
        ; pkg_digest = _
        } ->
      assert (Package.Name.equal pkg_digest.name info.name);
      let* platform = Lock_dir.Sys_vars.solver_env in
      let choose_for_current_platform field =
        Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform field ~platform
      in
      let* depends =
        Memo.parallel_map
          deps
          ~f:(fun { DB.Pkg_table.dep_pkg = _; dep_loc; dep_pkg_digest } ->
            let package_universe =
              match package_universe with
              | Dev_tool _ ->
                (* The dependencies of dev tools are installed into the default
                 context so they may be shared with the project's
                 dependencies. *)
                Package_universe.Dependencies Context_name.default
              | _ -> package_universe
            in
            resolve db dep_loc dep_pkg_digest package_universe)
      and+ files_dir =
        let+ lock_dir_path =
          Package_universe.lock_dir_path package_universe >>| Option.value_exn
        and+ lock_dir = Package_universe.lock_dir package_universe in
        let version =
          Option.some_if (Dune_pkg.Lock_dir.uses_versioned_paths lock_dir) info.version
        in
        Dune_pkg.Lock_dir.Pkg.files_dir info.name version ~lock_dir:lock_dir_path
        |> Path.as_in_build_dir_exn
      in
      let* mounted =
        match package_universe with
        | Dev_tool _ -> Memo.return None
        | Dependencies context ->
          let* candidate = Pkg_sources.find_candidate context info.name in
          (match candidate with
           | None -> Memo.return None
           | Some candidate ->
             let+ lock_dir = Lock_dir.get_exn context in
             let selected =
               DB.pkg_digest_of_name
                 lock_dir
                 platform
                 info.name
                 ~system_provided:db.system_provided
             in
             Option.some_if (Pkg_digest.equal pkg_digest selected) candidate)
      in
      let context = Package_universe.context_name package_universe in
      let* native =
        match mounted with
        | None -> Memo.return false
        | Some candidate ->
          let+ mounted =
            Pkg_sources.find_mounted context (Pkg_sources.Candidate.name candidate)
          in
          (match mounted with
           | None -> false
           | Some mounted -> Pkg_sources.Mounted.is_dune mounted)
      in
      let id = Pkg.Id.gen () in
      let write_paths =
        match mounted with
        | Some candidate -> Pkg_sources.Candidate.paths candidate
        | None -> Paths.make pkg_digest package_universe ~relative:Path.Build.relative
      in
      let install_command = choose_for_current_platform install_command in
      let install_command = Option.map install_command ~f:relocate in
      let build_command = choose_for_current_platform build_command in
      let build_command = Option.map build_command ~f:relocate_build in
      let paths =
        let paths = Paths.map_path write_paths ~f:Path.build in
        if native
        then (
          let prefix =
            Install_layout.root context (Package.Name.Set.singleton info.name)
            |> Path.build
          in
          let paths = Paths.with_prefix paths prefix in
          let candidate = Option.value_exn mounted in
          { paths with
            source_dir = Path.build (Pkg_sources.Candidate.artifact_root candidate)
          })
        else if
          (not (Pkg_toolchain.is_compiler_package_with_toolchains_enabled info.name))
          || is_compiler_version_relocatable info
          || has_relocatable_compiler_marker info depends
        then paths
        else (
          (* Modify the environment as well as build and install commands for
             the compiler package. The specific changes are:
             - setting the prefix in the build environment to inside the user's
               toolchain directory
             - changing the install command so that the
               package is installed with the DESTDIR variable set to a
               temporary directory, and the result is then moved to the user's
               toolchain directory
             - if a matching version of the compiler is
               already installed in the user's toolchain directory then the
               build and install commands are replaced with no-ops
             *)
          let prefix = Pkg_toolchain.installation_prefix pkg in
          let install_roots =
            Pkg_toolchain.install_roots ~prefix
            |> Install.Roots.map ~f:Path.outside_build_dir
          in
          { paths with
            prefix = Path.outside_build_dir prefix
          ; install_roots = Lazy.from_val install_roots
          ; install_paths =
              lazy
                (Install.Paths.make
                   ~relative:Path.relative
                   ~package:info.name
                   ~roots:install_roots)
          })
      in
      let t =
        { Pkg.id
        ; context
        ; native
        ; recipe =
            { Opam_build.Recipe.build = build_command
            ; install = install_command
            ; depexts
            ; exported_env
            }
        ; depends
        ; depends_on_dune = has_dune_dep
        ; paths
        ; write_paths
        ; info
        ; files_dir
        ; mounted
        ; pkg_digest
        ; exported_env = []
        }
      in
      let+ exported_env =
        let expander =
          Action_expander.expander (Package_universe.context_name package_universe) t
        in
        Memo.parallel_map exported_env ~f:(Action_expander.exported_env expander)
      in
      t.exported_env <- exported_env;
      Some t
  ;;

  let resolve =
    let memo =
      Memo.create
        "pkg-resolve"
        ~input:(module Input)
        ~human_readable_description:(fun t ->
          Pp.textf "- package %s" (Package.Name.to_string t.pkg_digest.name)
          |> Option.some)
        resolve_impl
    in
    fun (db : DB.t) loc pkg_digest package_universe ->
      Memo.exec memo { db; pkg_digest; universe = package_universe }
      >>| function
      | Some s -> s
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf "Unknown package %S" (Package.Name.to_string pkg_digest.name) ]
  ;;
end

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let source_rules (pkg : Pkg.t) =
  let+ source_deps, copy_rules =
    match pkg.info.source with
    | None -> Memo.return (Dep.Set.empty, [])
    | Some source ->
      let loc = fst source.url in
      Lock_dir.source_kind source
      >>= (function
       | `Local (`File, _) | `Fetch ->
         let fetch =
           Fetch_rules.fetch ~target:pkg.write_paths.source_dir `Directory source
           |> With_targets.map
                ~f:
                  (Action.Full.map ~f:(fun action ->
                     let progress =
                       Pkg_build_progress.progress_action
                         pkg.info.name
                         pkg.info.version
                         `Downloading
                     in
                     Action.progn [ progress; action ]))
         in
         Memo.return (Dep.Set.of_files [ pkg.paths.source_dir ], [ loc, fetch ])
       | `Local (`Directory, source_root) ->
         let+ source_files, rules =
           let source_root = Path.outside_build_dir source_root in
           Pkg.source_files pkg ~loc
           >>| Path.Local.Set.fold ~init:([], []) ~f:(fun file (source_files, rules) ->
             let src = Path.append_local source_root file in
             if Path.is_broken_symlink src
             then
               (* Don't copy broken symlinks into the build directory. Note
                  that this only works for packages sourced from local
                  directories. Packages whose source is extracted from an
                  archive (possibly fetched over the web) have broken symlinks
                  explicitly deleted immediately after the archive is
                  extracted. This logic is implemented in
                  [Fetch.resolve_directory_symlinks]. *)
               source_files, rules
             else (
               let dst = Path.Build.append_local pkg.write_paths.source_dir file in
               let copy = loc, Action_builder.copy ~src ~dst in
               Path.build dst :: source_files, copy :: rules))
         in
         Dep.Set.of_files source_files, rules)
  in
  let extra_source_deps, extra_copy_rules =
    List.map pkg.info.extra_sources ~f:(fun (local, (fetch : Source.t)) ->
      let extra_source = Paths.extra_source_build pkg.write_paths local in
      let rule =
        let loc = fst fetch.url in
        (* We assume that [fetch] is always a file. Would be good
           to give a decent error message if it's not *)
        match Source.kind fetch with
        | `Directory_or_archive src ->
          loc, Action_builder.copy ~src:(Path.external_ src) ~dst:extra_source
        | `Fetch ->
          let rule = Fetch_rules.fetch ~target:extra_source `File fetch in
          loc, rule
      in
      Path.build extra_source, rule)
    |> List.unzip
  in
  let copy_rules = copy_rules @ extra_copy_rules in
  let source_deps = Dep.Set.union source_deps (Dep.Set.of_files extra_source_deps) in
  source_deps, Memo.parallel_iter copy_rules ~f:(fun (loc, copy) -> rule ~loc copy)
;;

let rec scan_contents p =
  let module P = Path.Build in
  let dir_contents =
    match Readdir.read_directory_with_kinds (P.to_string p) with
    | Error (Unix.ENOENT, _, _) ->
      (* Directory doesn't exist - package has no extra files *)
      []
    | Ok dir_contents -> dir_contents
    | Error e ->
      Code_error.raise
        "Failure to enumerate files"
        [ "error", Unix_error.Detailed.to_dyn e ]
  in
  List.fold_left
    dir_contents
    ~init:(P.Set.empty, P.Set.empty)
    ~f:(fun (files, empty_directories) (file_name, file_kind) ->
      let p = P.relative_fname p file_name in
      match (file_kind : Unix.file_kind) with
      | S_REG -> P.Set.add files p, empty_directories
      | S_DIR ->
        let recursive_files, recursive_empty_dir = scan_contents p in
        (match P.Set.is_empty recursive_files, P.Set.is_empty recursive_empty_dir with
         | true, true ->
           recursive_files, P.Set.union empty_directories recursive_empty_dir
         | true, false -> files, P.Set.union empty_directories recursive_empty_dir
         | false, _ -> P.Set.union files recursive_files, empty_directories)
      | otherwise ->
        Code_error.raise
          "Unsupported directory content"
          [ "path", P.to_dyn p; "file_kind", File_kind.to_dyn otherwise ])
;;

let files path =
  let files, empty_directories = scan_contents path in
  let to_path_set set =
    Path.Build.Set.fold
      set
      ~f:(fun e acc -> Path.Set.add acc (Path.build e))
      ~init:Path.Set.empty
  in
  let files = to_path_set files in
  let empty_directories = to_path_set empty_directories in
  Dep.Set.of_source_files ~files ~empty_directories, files
;;

let dune_dep =
  lazy (Sys.executable_name |> Path.External.of_string |> Path.external_ |> Dep.file)
;;

let build_rule context_name ~source_deps (pkg : Pkg.t) =
  let source =
    let open Action_builder.O in
    let* source =
      match pkg.mounted with
      | None ->
        Action_builder.return { Opam_build.Source_input.files = []; directories = [] }
      | Some candidate -> Action_builder.of_memo (Pkg_sources.source candidate)
    in
    let+ files =
      Action_builder.of_memo
        (Memo.of_thunk (fun () ->
           let deps, files = files pkg.files_dir in
           Memo.return (files, deps)))
      |> Action_builder.dyn_deps
    in
    let files =
      source.files
      @ Path.Set.to_list_map files ~f:(fun src ->
        src, Path.drop_prefix_exn src ~prefix:(Path.build pkg.files_dir))
      @ List.map pkg.info.extra_sources ~f:(fun (local, source) ->
        let path =
          match pkg.mounted with
          | None -> Paths.extra_source pkg.paths local
          | Some _ -> Fetch_rules.target source `File |> Path.build
        in
        path, local)
    in
    { source with files }
  in
  let progress =
    Pkg_build_progress.progress_action pkg.info.name pkg.info.version `Building
    |> Action.Full.make
    |> Action_builder.With_targets.return
  in
  let build =
    let open Action_builder.O in
    let* expander =
      match pkg.mounted with
      | None -> Action_builder.return (Action_expander.expander context_name pkg)
      | Some _ ->
        let* dependencies =
          Action_builder.List.map (Pkg.deps_closure pkg) ~f:Pkg.dependency
        in
        let* layout = Install_layout.materialize context_name dependencies in
        let path = Env_path.path (Global.env ()) in
        let+ env =
          match dependencies with
          | [] -> Action_builder.return (Global.env ())
          | _ -> Install_layout.env layout ~path ~base:(Global.env ())
        in
        Opam_build.Action_expander.for_recipe
          ~context:context_name
          ~path
          ~paths:pkg.paths
          ~version:pkg.info.version
          ~variables:(Pkg_info.variables pkg.info)
          ~depexts:pkg.recipe.depexts
          ~env
          ~direct:(Package.Name.Set.of_list_map pkg.depends ~f:(fun pkg -> pkg.info.name))
          ~dependencies:(Install_layout.dependencies layout)
    in
    let* action =
      Action_builder.of_memo
        (Opam_build.execute_recipe
           pkg.recipe
           ~expander
           ~write_paths:pkg.write_paths
           ~source)
    in
    let deps =
      if pkg.depends_on_dune
      then Dep.Set.add source_deps (Lazy.force dune_dep)
      else source_deps
    in
    let* () = Action_builder.deps deps
    and* () = Pkg.package_deps pkg in
    let { Action_builder.With_targets.build; targets = _ } =
      Action_builder.progn [ progress; action ]
    in
    build
  in
  Action_builder.with_no_targets build
  |> Action_builder.With_targets.add_directories
       ~directory_targets:[ pkg.write_paths.target_dir ]
  |> Memo.return
;;

let gen_rules context_name (pkg : Pkg.t) =
  let* source_deps, copy_rules =
    match pkg.mounted with
    | None -> source_rules pkg
    | Some _ -> Memo.return (Dep.Set.empty, Memo.return ())
  in
  let* () = copy_rules
  and* build_rule = build_rule context_name pkg ~source_deps in
  rule ~loc:Loc.none (* TODO *) build_rule
;;

module Gen_rules = Build_config.Gen_rules

let pkg_alias_disabled =
  Action_builder.fail
    { fail =
        (fun () ->
          let error =
            [ Pp.text "The @pkg-install alias cannot be used without a lock dir" ]
          in
          let hints =
            [ Pp.concat
                ~sep:Pp.space
                [ Pp.text "You might want to create the lock dir with"
                ; User_message.command "dune pkg lock"
                ]
            ]
          in
          User_error.raise ~hints error)
    }
;;

let setup_pkg_install_alias =
  let build_packages_of_context ctx_name =
    (* Fetching the package target implies that we will also fetch the extra
       sources. *)
    let open Action_builder.O in
    let* packages =
      Action_builder.of_memo
        (let open Memo.O in
         let* db = DB.of_ctx ctx_name ~allow_sharing:false in
         Pkg_digest.Map.values db.pkg_digest_table
         |> Memo.parallel_map ~f:(fun { DB.Pkg_table.pkg_digest; _ } ->
           Resolve.resolve db Loc.none pkg_digest (Dependencies ctx_name)))
    in
    Action_builder.List.iter packages ~f:(fun pkg ->
      Action_builder.ignore (Pkg.dependency pkg))
  in
  fun ~dir ctx_name ->
    (* We only need to build when the build_dir is the root of the context *)
    match
      let build_dir = Context_name.build_dir ctx_name in
      Path.Build.equal dir build_dir
    with
    | false -> Gen_rules.no_rules
    | true ->
      let rule =
        let* active = Lock_dir.lock_dir_active ctx_name in
        let alias = Alias.make ~dir Alias0.pkg_install in
        Rules.collect_unit (fun () ->
          let deps =
            match active with
            | true -> build_packages_of_context ctx_name
            | false -> pkg_alias_disabled
          in
          Rules.Produce.Alias.add_deps alias deps)
      in
      Gen_rules.make
        ~build_dir_only_sub_dirs:
          (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty)
        rule
;;

let setup_package_rules db ~package_universe ~dir ~pkg_digest : Gen_rules.t Memo.t =
  let* pkg = Resolve.resolve db Loc.none pkg_digest package_universe in
  match pkg.mounted with
  | Some _ -> Memo.return Gen_rules.no_rules
  | None ->
    let paths =
      Paths.make pkg.pkg_digest package_universe ~relative:Path.Build.relative
    in
    let+ directory_targets =
      let map =
        let target_dir = paths.target_dir in
        Path.Build.Map.singleton target_dir Loc.none
      in
      match pkg.info.source with
      | None -> Memo.return map
      | Some source ->
        Lock_dir.source_kind source
        >>| (function
         | `Local (`Directory, _) -> map
         | `Local (`File, _) | `Fetch ->
           Path.Build.Map.add_exn map paths.source_dir (fst source.url))
    in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty
    in
    let context_name = Package_universe.context_name package_universe in
    let rules = Rules.collect_unit (fun () -> gen_rules context_name pkg) in
    Gen_rules.make ~directory_targets ~build_dir_only_sub_dirs rules
;;

let setup_rules ~components ~dir ctx =
  (* Note that the path components in the following patterns must
     correspond to the paths returned by [Paths.make]. The string
     ".dev-tool" is hardcoded into several patterns, and must match
     the value of [Pkg_dev_tool.install_path_base_dir_name]. *)
  assert (String.equal Pkg_dev_tool.install_path_base_dir_name ".dev-tool");
  match Context_name.is_default ctx, components with
  | true, [ ".dev-tool"; dev_tool_package_name ] ->
    let pkg_name = Package.Name.of_string dev_tool_package_name in
    let dev_tool = Pkg_dev_tool.of_package_name pkg_name in
    let* db, pkg_digest = DB.of_dev_tool (Dune_pkg.Dev_tool.of_package_name pkg_name) in
    setup_package_rules db ~package_universe:(Dev_tool dev_tool) ~dir ~pkg_digest
  | true, [ ".dev-tool" ] -> Gen_rules.make_empty ~dir Subdir_set.all |> Memo.return
  | _, [ ".pkg" ] -> Gen_rules.make_empty ~dir Subdir_set.all |> Memo.return
  | _, [ ".pkg"; pkg_digest_string ] ->
    (* Only generate pkg rules if there is a lock dir for that context *)
    let* lock_dir_active = Lock_dir.lock_dir_active ctx in
    (match lock_dir_active with
     | false -> Memo.return @@ Gen_rules.make (Memo.return Rules.empty)
     | true ->
       let pkg_digest = Pkg_digest.of_string pkg_digest_string in
       let* db = DB.of_ctx ctx ~allow_sharing:true in
       setup_package_rules db ~package_universe:(Dependencies ctx) ~dir ~pkg_digest)
  | _, ".pkg" :: _ :: _ ->
    Memo.return (Gen_rules.Redirect_to_parent Gen_rules.Rules.empty)
  | true, ".dev-tool" :: _ :: _ :: _ ->
    Memo.return (Gen_rules.Redirect_to_parent Gen_rules.Rules.empty)
  | is_default, [] ->
    let sub_dirs =
      Filename.pkg_dir_basename
      :: (if is_default then [ Filename.dev_tool_dir_basename ] else [])
    in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return Gen_rules.no_rules
;;

let resolve_pkg_dep context (loc, package_name) =
  let* db, pkg_digest = DB.of_project_pkg context package_name in
  Resolve.resolve db loc pkg_digest (Dependencies context)
;;

let dependency context name =
  let* candidate = Pkg_sources.find_candidate context name in
  match candidate with
  | None -> Memo.return None
  | Some _ ->
    let+ pkg = resolve_pkg_dep context (Loc.none, name) in
    Some (Pkg.dependency pkg)
;;

let setup_mounted_rules context ~dir components =
  let open Gen_rules in
  let module Rules = Dune_engine.Rules in
  match components with
  | [] ->
    make_empty
      ~dir
      (Subdir_set.of_list [ Filename.of_string_exn Pkg_sources.artifact_dir_basename ])
    |> Memo.return
  | [ packages ] when String.equal packages Pkg_sources.artifact_dir_basename ->
    let+ candidates = Pkg_sources.candidates context in
    List.map candidates ~f:(fun candidate ->
      Pkg_sources.Candidate.name candidate
      |> Package.Name.to_string
      |> Filename.of_string_exn)
    |> Subdir_set.of_list
    |> make_empty ~dir
  | packages :: name :: rest when String.equal packages Pkg_sources.artifact_dir_basename
    ->
    let name = Package.Name.of_string name in
    let* candidate = Pkg_sources.find_candidate context name in
    (match candidate with
     | None -> Memo.return no_rules
     | Some candidate ->
       let paths = Pkg_sources.Candidate.paths candidate in
       let package = Package.Name.to_string name in
       (match rest with
        | [] ->
          let rules =
            Rules.collect_unit (fun () ->
              Rules.Produce.Alias.add_deps
                (Alias.make Alias0.all ~dir)
                (Action_builder.path (Path.build paths.target_dir)))
          in
          make
            ~build_dir_only_sub_dirs:
              (Build_only_sub_dirs.singleton
                 ~dir
                 (Subdir_set.of_list [ Filename.of_string_exn ".opam" ]))
            rules
          |> Memo.return
        | [ ".opam" ] ->
          make_empty ~dir (Subdir_set.of_list [ Filename.of_string_exn package ])
          |> Memo.return
        | [ ".opam"; owner ] when String.equal owner package ->
          let rules =
            Rules.collect_unit (fun () ->
              let* pkg = resolve_pkg_dep context (Loc.none, name) in
              gen_rules context pkg)
          in
          make
            ~directory_targets:(Path.Build.Map.singleton paths.target_dir Loc.none)
            rules
          |> Memo.return
        | _ -> Memo.return no_rules))
  | _ -> Memo.return no_rules
;;

let ocaml_toolchain context =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf
      "Loading OCaml toolchain from Lock directory for context %S"
      (Context_name.to_string context))
  @@ fun () ->
  let* lock_dir = Lock_dir.get_exn context in
  match lock_dir.ocaml with
  | None -> Memo.return None
  | Some ocaml ->
    let+ pkg = resolve_pkg_dep context ocaml in
    let toolchain =
      let open Action_builder.O in
      let transitive_deps = pkg :: Pkg.deps_closure pkg in
      let* env, binaries =
        Action_builder.List.fold_left
          ~init:(Global.env (), Path.Set.empty)
          ~f:(fun (env, binaries) pkg ->
            let env = Env.extend_env env (Pkg.exported_env pkg) in
            let+ cookie = (Pkg_installed.of_paths pkg.paths).cookie in
            let binaries =
              Section.Map.find cookie.files Bin
              |> Option.value ~default:[]
              |> Path.Set.of_list
              |> Path.Set.union binaries
            in
            env, binaries)
          transitive_deps
      in
      let path = Env_path.path (Global.env ()) in
      Action_builder.of_memo @@ Ocaml_toolchain.of_binaries ~path context env binaries
    in
    Some (Action_builder.memoize "ocaml_toolchain" toolchain)
;;

let all_deps universe =
  let* db =
    match (universe : Package_universe.t) with
    | Dependencies ctx ->
      (* Disallow sharing so that the only packages in the DB are the ones from
         the universe's respective lock directory. *)
      DB.of_ctx ctx ~allow_sharing:false
    | Dev_tool tool -> DB.of_dev_tool tool >>| fst
  in
  Pkg_digest.Map.values db.pkg_digest_table
  |> Memo.parallel_map ~f:(fun { DB.Pkg_table.pkg_digest; _ } ->
    Resolve.resolve db Loc.none pkg_digest universe)
  >>| Pkg.top_closure
;;

let all_project_deps context = all_deps (Dependencies context)

let project_deps ~(packages : Package.Name.Selection.t) context =
  let+ all_project_deps = all_project_deps context in
  match packages with
  | All -> all_project_deps
  | Only packages ->
    List.filter all_project_deps ~f:(fun (pkg : Pkg.t) ->
      Package.Name.Set.mem packages pkg.info.name)
;;

let describe_packages : Package.Name.Selection.t -> string = function
  | All -> "all packages"
  | Only packages ->
    Package.Name.Set.to_list packages
    |> List.map ~f:Package.Name.to_string
    |> String.concat ~sep:", "
;;

module Context_and_packages = struct
  type t = Context_name.t * Package.Name.Selection.t

  let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn Package.Name.Selection.to_dyn
  let equal = Tuple.T2.equal Context_name.equal Package.Name.Selection.equal
  let hash = Tuple.T2.hash Context_name.hash Package.Name.Selection.hash
end

let lock_dir_binaries =
  Memo.create
    "lock-directory-binaries"
    ~input:(module Context_and_packages)
    ~human_readable_description:(fun (context, packages) ->
      Some
        (Pp.textf
           "Loading the binaries of %s in the lock directory for %S"
           (describe_packages packages)
           (Context_name.to_string context)))
    (fun (context, packages) ->
       let* packages = project_deps ~packages context in
       let native, opaque =
         List.partition_map packages ~f:(fun (pkg : Pkg.t) ->
           if pkg.native then Left pkg else Right pkg)
       in
       let* { binaries; dep_info = _ } =
         Action_expander.Artifacts_and_deps.of_closure opaque
       in
       let+ native =
         Package.Name.Set.of_list_map native ~f:(fun (pkg : Pkg.t) -> pkg.info.name)
         |> Install_layout.binaries context
       in
       Filename.Map.superpose native binaries)
;;

let which ~packages context program =
  let+ binaries = Memo.exec lock_dir_binaries (context, packages) in
  Filename.Map.find binaries program
;;

let ocamlpath universe =
  let+ all_project_deps = all_deps universe in
  let env =
    List.filter all_project_deps ~f:(fun (pkg : Pkg.t) -> not pkg.native)
    |> Pkg.build_env_of_deps
  in
  Env.Map.find env Dune_findlib.Config.ocamlpath_var
  |> Option.value ~default:[]
  |> List.map ~f:(function
    | Value.Dir p | Path p -> p
    | String s -> Path.of_filename_relative_to_initial_cwd s)
;;

let project_ocamlpath context = ocamlpath (Dependencies context)
let dev_tool_ocamlpath dev_tool = ocamlpath (Dev_tool dev_tool)
let lock_dir_active = Lock_dir.lock_dir_active
let lock_dir_path = Lock_dir.get_path

let dev_tool_env tool =
  let package_name = Dune_pkg.Dev_tool.package_name tool in
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf
      "lock directory environment for dev tools %S"
      (Package.Name.to_string package_name))
  @@ fun () ->
  let* db, pkg_digest = DB.of_dev_tool tool in
  let+ pkg = Resolve.resolve db Loc.none pkg_digest (Dev_tool tool) in
  Pkg.exported_env pkg
;;

let exported_env context =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf "lock directory environment for context %S" (Context_name.to_string context))
  @@ fun () ->
  let+ all_project_deps = all_project_deps context in
  let env = Pkg.build_env_of_deps all_project_deps in
  let vars = Env.Map.map env ~f:Value_list_env.string_of_env_values in
  Env.extend Env.empty ~vars
;;

let lock_dir_package_env =
  Memo.create
    "lock-directory-package-env"
    ~input:(module Context_and_packages)
    ~human_readable_description:(fun (context, packages) ->
      Some
        (Pp.textf
           "lock directory environment of %s for context %S"
           (describe_packages packages)
           (Context_name.to_string context)))
    (fun (context, packages) ->
       lock_dir_active context
       >>= function
       | false -> Memo.return Env.empty
       | true ->
         let+ deps = project_deps ~packages context in
         let vars =
           Pkg.build_env_of_deps deps
           |> Env.Map.map ~f:Value_list_env.string_of_env_values
         in
         Env.extend Env.empty ~vars)
;;

let env_for_packages ~(packages : Package.Name.Selection.t) context =
  Memo.exec lock_dir_package_env (context, packages)
;;

let bin_paths_for_packages ~packages context =
  let+ env = env_for_packages ~packages context in
  Env_path.path env
;;

let find_package ctx pkg =
  lock_dir_active ctx
  >>= function
  | false -> Memo.return None
  | true ->
    let+ pkg = resolve_pkg_dep ctx (Loc.none, pkg) in
    Some
      (let open Action_builder.O in
       let* () = Pkg.package_deps pkg in
       Action_builder.ignore (Pkg.dependency pkg))
;;

let package_prefixes ctx =
  lock_dir_active ctx
  >>= function
  | false -> Memo.return []
  | true ->
    all_project_deps ctx
    >>| List.map ~f:(fun { Pkg.info = { name; _ }; paths = { Paths.prefix; _ }; _ } ->
      prefix, name)
;;

let resolve_installed_file ~loc ~context_name ~pkg_name ~section ~file =
  let open Action_builder.O in
  let* pkg = Action_builder.of_memo (resolve_pkg_dep context_name (loc, pkg_name)) in
  let* { Opam_build.Dependency.paths; files; _ } = Pkg.dependency pkg in
  let section_dir =
    let install_paths = Lazy.force paths.install_paths in
    Install.Paths.get install_paths section
  in
  let path = Path.append_local section_dir file in
  let installed = Section.Map.find files section |> Option.value ~default:[] in
  match List.exists installed ~f:(Path.equal path) with
  | true ->
    let+ () = Action_builder.path path in
    path
  | false ->
    let file_str = Path.Local.to_string file in
    let candidates =
      List.filter_map installed ~f:(Path.drop_prefix ~prefix:section_dir)
      |> List.map ~f:Path.Local.to_string
    in
    User_error.raise
      ~loc
      ~hints:(User_message.did_you_mean file_str ~candidates)
      [ Pp.textf
          "File %s not found in section %s of package %s"
          file_str
          (Section.to_string section)
          (Package.Name.to_string pkg_name)
      ]
;;

let all_filtered_depexts context =
  let* all_project_deps = all_project_deps context in
  Memo.List.map all_project_deps ~f:(fun (pkg : Pkg.t) ->
    let expander = Action_expander.expander context pkg in
    Action_expander.Expander.filtered_depexts expander)
  >>| List.concat
  >>| List.sort_uniq ~compare:String.compare
;;

let pkg_digest_of_project_dependency ctx package_name =
  let+ db = DB.of_ctx ctx ~allow_sharing:false in
  Pkg_digest.Map.keys db.pkg_digest_table
  |> List.find ~f:(fun (pkg_digest : Pkg_digest.t) ->
    Package.Name.equal pkg_digest.name package_name)
;;
