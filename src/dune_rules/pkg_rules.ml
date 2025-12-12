open Import
open Memo.O

include struct
  open Dune_pkg
  module Package_variable = Package_variable
  module Substs = Substs
  module Checksum = Checksum
  module Source = Source
  module Build_command = Lock_dir.Build_command
  module Display = Dune_engine.Display
  module Pkg_info = Lock_dir.Pkg_info
  module Depexts = Lock_dir.Depexts
  module Digest_feed = Dune_digest.Feed
end

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  let dyn_of_value : value -> Dyn.t =
    let open Dyn in
    function
    | B b -> variant "Bool" [ bool b ]
    | S s -> variant "String" [ string s ]
    | L xs -> variant "Strings" [ list string xs ]
  ;;

  let dune_value : value -> Value.t list = function
    | B b -> [ String (Bool.to_string b) ]
    | S s -> [ String s ]
    | L s -> List.map s ~f:(fun x -> Value.String x)
  ;;

  let of_values : dir:Path.t -> Value.t list -> value =
    fun ~dir xs ->
    match List.map xs ~f:(Value.to_string ~dir) with
    | [ x ] -> S x
    | xs -> L xs
  ;;

  let to_dyn (name, value) =
    Dyn.(pair Package_variable_name.to_dyn dyn_of_value (name, value))
  ;;
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
      (* CR-Leonidas-from-XIV: It probably isn't always [Some] *)
      dev_tool
      |> Lock_dir.dev_tool_external_lock_dir
      |> Path.external_
      |> Option.some
      |> Memo.return
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

  let digest_feed =
    Digest_feed.tuple3
      Package.Name.digest_feed
      Package_version.digest_feed
      Digest_feed.digest
    |> Digest_feed.contramap ~f:(fun { name; version; lockfile_and_dependency_digest } ->
      name, version, lockfile_and_dependency_digest)
  ;;

  let create lockfile_pkg depends_pkg_digests =
    let lockfile_and_dependency_digest =
      Digest_feed.compute_digest
        (Digest_feed.tuple2 Lock_dir.Pkg.digest_feed (Digest_feed.list digest_feed))
        (Dune_pkg.Lock_dir.Pkg.remove_locs lockfile_pkg, depends_pkg_digests)
    in
    let name = lockfile_pkg.info.name in
    let version = lockfile_pkg.info.version in
    { name; version; lockfile_and_dependency_digest }
  ;;
end

module Paths = struct
  (* The [paths] of a package are the information about the artifacts
     that we know {e without} executing any commands. *)
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; extra_sources : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  let map_path t ~f =
    { t with
      source_dir = f t.source_dir
    ; target_dir = f t.target_dir
    ; extra_sources = f t.extra_sources
    ; install_roots = Lazy.map ~f:(Install.Roots.map ~f) t.install_roots
    ; install_paths = Lazy.map ~f:(Install.Paths.map ~f) t.install_paths
    ; prefix = f t.prefix
    }
  ;;

  let install_roots ~target_dir ~relative =
    Install.Roots.opam_from_prefix ~relative target_dir
  ;;

  let install_paths roots package ~relative = Install.Paths.make ~relative ~package ~roots

  let of_root name ~root ~relative =
    let source_dir = relative root "source" in
    let target_dir = relative root "target" in
    let extra_sources = relative root "extra_source" in
    let install_roots = lazy (install_roots ~target_dir ~relative) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name ~relative) in
    { source_dir
    ; target_dir
    ; extra_sources
    ; name
    ; install_paths
    ; install_roots
    ; prefix = target_dir
    }
  ;;

  let extra_source t extra_source = Path.append_local t.extra_sources extra_source

  let extra_source_build t extra_source =
    Path.Build.append_local t.extra_sources extra_source
  ;;

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

  let make_install_cookie target_dir ~relative = relative target_dir "cookie"

  let install_cookie' target_dir =
    make_install_cookie target_dir ~relative:Path.Build.relative
  ;;

  let install_cookie t = make_install_cookie t.target_dir ~relative:Path.relative

  let install_file t =
    Path.Build.relative
      t.source_dir
      (sprintf "%s.install" (Package.Name.to_string t.name))
  ;;

  let config_file t =
    Path.Build.relative t.source_dir (sprintf "%s.config" (Package.Name.to_string t.name))
  ;;

  let install_paths t = Lazy.force t.install_paths
  let install_roots t = Lazy.force t.install_roots
  let target_dir t = t.target_dir
end

module Install_cookie = struct
  (* The install cookie represents a serialized representation of all the
     installed artifacts and variables.

     The install cookie of a package is the source of all data we must refer to
     address a package's artifacts.

     It is constructed after we've built and installed the packages. In this
     sense, it is the "installation trace" that we must refer to so that we
     don't have to know anything about the installation procedure.
  *)

  module Gen = struct
    type 'files t =
      { files : 'files
      ; variables : Variable.t list
      }

    let to_dyn f { files; variables } =
      let open Dyn in
      record [ "files", f files; "variables", list Variable.to_dyn variables ]
    ;;
  end

  type t = Path.t list Section.Map.t Gen.t

  module Persistent = Persistent.Make (struct
      type nonrec t = (Section.t * Path.t list) list Gen.t

      let name = "INSTALL-COOKIE"
      let version = 3

      let to_dyn =
        let open Dyn in
        Gen.to_dyn (list (pair Section.to_dyn (list Path.to_dyn)))
      ;;

      let test_example () = { Gen.files = []; variables = [] }
    end)

  let load_exn f =
    match Persistent.load f with
    | Some f -> { f with files = Section.Map.of_list_exn f.files }
    | None -> User_error.raise ~loc:(Loc.in_file f) [ Pp.text "unable to load" ]
  ;;

  let dump path (t : t) =
    Persistent.dump path { t with files = Section.Map.to_list t.files }
  ;;
end

module Value_list_env = struct
  (* A representation of an environment where each variable can hold a
     list of [Value.t]. Each variable will be encoded into a delimited
     string (in the style of the PATH variable). *)
  type t = Value.t list Env.Map.t

  let global : t Lazy.t =
    let parse_strings s = Bin.parse s |> List.map ~f:(fun s -> Value.String s) in
    let of_env env : t = Env.to_map env |> Env.Map.map ~f:parse_strings in
    lazy (of_env (Global.env ()))
  ;;

  (* Concatenate a list of values in the style of lists found in
     environment variables, such as PATH *)
  let string_of_env_values values =
    List.map values ~f:(function
      | Value.String s -> s
      | Dir s | Path s -> Path.to_absolute_filename s)
    |> Bin.encode_strings
  ;;

  let to_env (t : t) = Env.Map.map t ~f:string_of_env_values |> Env.of_map
  let get_path t = Env.Map.find t Env_path.var

  (* [extend_concat_path a b] adds all variables from [b] to [a]
     overwriting any existing values of those variables in [a] except for PATH
     which is set to the concatenation of the PATH variables from [a] and [b]
     with the PATH entries from [b] preceding the PATH entries from
     [a]. If only one of the arguments contains a PATH variable then
     its value will be the value of PATH in the result, however if
     neither argument contains a PATH variable then PATH will be unset
     in the result. *)
  let extend_concat_path a b =
    let extended = Env.Map.superpose b a in
    let concated_path =
      match get_path a, get_path b with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b -> Some (b @ a)
    in
    match concated_path with
    | None -> extended
    | Some concated_path -> Env.Map.set extended Env_path.var concated_path
  ;;

  (* Adds a path to an env where variables are associated with lists
     of paths. The path is prepended to the list associated with the
     given variable and a new binding is added to the env if the
     variable is not yet part of the env. *)
  let add_path (t : t) var path : t =
    Env.Map.update t var ~f:(fun paths ->
      let paths = Option.value paths ~default:[] in
      Some (Value.Dir path :: paths))
  ;;
end

module Env_update = struct
  include Dune_lang.Action.Env_update

  (* Handle the :=, +=, =:, and =+ opam environment update operators.

     The operators with colon character update a variable, adding a
     leading/trailing separator (e.g. the ':' chars in PATH on unix)
     if the variable was initially unset or empty, while the operators
     with a plus character add no leading/trailing separator in such a
     case.

     Updates where the newly added value is the empty string are
     ignored since opam refuses to add empty strings to list
     variables.*)
  let update kind ~new_v ~old_v ~f =
    if new_v = ""
    then old_v
    else (
      match kind with
      | `Colon ->
        let old_v = Option.value ~default:[] old_v in
        Some (f ~old_v ~new_v)
      | `Plus ->
        (match old_v with
         | None | Some [] -> Some [ Value.String new_v ]
         | Some old_v -> Some (f ~old_v ~new_v)))
  ;;

  let append = update ~f:(fun ~old_v ~new_v -> old_v @ [ Value.String new_v ])
  let prepend = update ~f:(fun ~old_v ~new_v -> Value.String new_v :: old_v)

  let set env { op; var = k; value = new_v } =
    Env.Map.update env k ~f:(fun old_v ->
      let append = append ~new_v ~old_v in
      let prepend = prepend ~new_v ~old_v in
      match op with
      | Eq ->
        if new_v = ""
        then if Sys.win32 then None else Some [ String "" ]
        else Some [ Value.String new_v ]
      | PlusEq -> prepend `Plus
      | ColonEq -> prepend `Colon
      | EqPlus -> append `Plus
      | EqColon -> append `Colon
      | EqPlusEq ->
        (* TODO nobody uses this AFAIK *)
        assert false)
  ;;
end

module Pkg = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; build_command : Build_command.t option
    ; install_command : Dune_lang.Action.t option
    ; depends : t list
    ; depexts : Depexts.t list
    ; info : Pkg_info.t
    ; paths : Path.t Paths.t
    ; write_paths : Path.Build.t Paths.t
    ; files_dir : Path.Build.t option
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
    let skip_file = String.is_prefix ~prefix:".#" in
    let rec loop root acc path =
      let full_path = Path.External.append_local root path in
      Fs_memo.dir_contents (External full_path)
      >>= function
      | Error e ->
        User_error.raise
          ~loc
          [ Pp.textf "Unable to read %s" (Path.External.to_string_maybe_quoted full_path)
          ; Unix_error.Detailed.pp e
          ]
      | Ok contents ->
        let files, dirs =
          let contents = Fs_cache.Dir_contents.to_list contents in
          List.rev_filter_partition_map contents ~f:(fun (name, kind) ->
            (* TODO handle links and cycles correctly *)
            match kind with
            | S_DIR -> if skip_dir name then Skip else Right name
            | _ -> if skip_file name then Skip else Left name)
        in
        let acc =
          Path.Local.Set.of_list_map files ~f:(Path.Local.relative path)
          |> Path.Local.Set.union acc
        in
        let+ dirs =
          Memo.parallel_map dirs ~f:(fun dir ->
            let dir = Path.Local.relative path dir in
            loop root Path.Local.Set.empty dir)
        in
        Path.Local.Set.union_all (acc :: dirs)
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

  let dep t = Dep.file t.paths.target_dir

  let package_deps t =
    deps_closure t
    |> List.fold_left ~init:Dep.Set.empty ~f:(fun acc t -> dep t |> Dep.Set.add acc)
  ;;

  let install_roots t =
    let default_install_roots = Paths.install_roots t.paths in
    match Pkg_toolchain.is_compiler_and_toolchains_enabled t.info.name with
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
      ; "CDPATH", [ Value.String "" ]
      ; "MAKELEVEL", [ Value.String "" ]
      ; "OPAM_PACKAGE_NAME", [ Value.String (Package.Name.to_string t.info.name) ]
      ; ( "OPAM_PACKAGE_VERSION"
        , [ Value.String (Package_version.to_string t.info.version) ] )
      ; "OPAMCLI", [ Value.String "2.0" ]
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

module Expander0 = struct
  include Expander0

  type t =
    { name : Dune_pkg.Package_name.t
    ; paths : Path.t Paths.t
    ; artifacts : Path.t Filename.Map.t Memo.t
    ; depends :
        (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
          Memo.t
    ; depexts : Depexts.t list
    ; context : Context_name.t
    ; version : Package_version.t
    ; env : Value.t list Env.Map.t
    }

  let expand_pform_fdecl
    : (t
       -> source:Dune_sexp.Template.Pform.t
       -> Pform.t
       -> (Value.t list, [ `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t)
        Fdecl.t
    =
    Fdecl.create Dyn.opaque
  ;;
end

module Substitute = struct
  include Substs.Make (Memo)
  module Expander = Expander0

  module Spec = struct
    type ('src, 'dst) t =
      { (* XXX it's not good to serialize the substitution map like this. We're
           essentially implementing the same substitution procedure but in two
           different places: action geeneration, and action execution.

           The two implementations are bound to drift. Better would be to
           reconstruct everything that is needed to call our one and only
           substitution function. *)
        expander : Expander.t
      ; depends :
          (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
      ; artifacts : Path.t Filename.Map.t
      ; src : 'src
      ; dst : 'dst
      }

    let name = "substitute"
    let version = 3
    let bimap t f g = { t with src = f t.src; dst = g t.dst }
    let is_useful_to ~memoize = memoize

    let encode { expander; depends; artifacts; src; dst } input output : Sexp.t =
      let e =
        let paths (p : Path.t Paths.t) = p.source_dir, p.target_dir, p.name in
        ( paths expander.paths
        , String.Map.to_list artifacts
        , Package.Name.Map.to_list_map depends ~f:(fun _ (m, p) -> m, paths p)
        , expander.version )
        |> Digest.generic
        |> Digest.to_string_raw
      in
      List [ Atom e; input src; output dst ]
    ;;

    let action { expander; depends = _; artifacts = _; src; dst } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* () = Fiber.return () in
      let env (var : Substs.Variable.t) =
        let open Memo.O in
        ((* TODO loc *)
         let loc = Loc.none in
         let source =
           (* TODO it's rather ugly that we're going through the pform machinery
              to do this *)
           { Dune_sexp.Template.Pform.loc; name = ""; payload = None }
         in
         match
           match var with
           | Package var -> Some (Package_variable.to_pform var)
           | Global n ->
             Package_variable_name.to_string n
             |> Pform.Var.of_opam_global_variable_name
             |> Option.map ~f:(fun v -> Pform.Var v)
         with
         | None -> Memo.return @@ Variable.S ""
         | Some pform ->
           (Fdecl.get Expander.expand_pform_fdecl) expander ~source pform
           >>| (function
            | Error (`Undefined_pkg_var _) ->
              (* these are opam's semantics as far as I understand. *)
              Variable.S ""
            | Ok v ->
              let dir = Path.parent_exn src |> Path.drop_optional_sandbox_root in
              Variable.of_values v ~dir))
        >>| Option.some
      in
      subst env expander.paths.name ~src ~dst |> Memo.run
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action (expander : Expander.t) ~src ~dst =
    let+ depends = expander.depends
    and+ artifacts = expander.artifacts in
    A.action { Spec.expander; depends; artifacts; src; dst }
  ;;
end

module Action_expander = struct
  module Expander = struct
    include Expander0

    let map_exe _ x =
      (* TODO *)
      x
    ;;

    let dune_section_of_pform : Pform.Var.Pkg.Section.t -> Section.t = function
      | Lib -> Lib
      | Libexec -> Libexec
      | Bin -> Bin
      | Sbin -> Sbin
      | Toplevel -> Toplevel
      | Share -> Share
      | Etc -> Etc
      | Doc -> Doc
      | Stublibs -> Stublibs
      | Man -> Man
    ;;

    let section_dir_of_root
          (roots : _ Install.Roots.t)
          (section : Pform.Var.Pkg.Section.t)
      =
      match section with
      | Lib -> roots.lib_root
      | Libexec -> roots.libexec_root
      | Bin -> roots.bin
      | Sbin -> roots.sbin
      | Share -> roots.share_root
      | Etc -> roots.etc_root
      | Doc -> roots.doc_root
      | Man -> roots.man
      | Toplevel -> Path.relative roots.lib_root "toplevel"
      | Stublibs -> Path.relative roots.lib_root "stublibs"
    ;;

    let sys_poll_var accessor =
      accessor Lock_dir.Sys_vars.poll
      |> Memo.Lazy.force
      >>| function
      | Some v -> [ Value.String v ]
      | None ->
        (* TODO: in OPAM an unset variable evaluates to false, but we
           can't represent that in a string so it evaluates to an empty
           string instead *)
        [ Value.String "" ]
    ;;

    let expand_pkg (paths : Path.t Paths.t) (pform : Pform.Var.Pkg.t) =
      match pform with
      | Switch -> Memo.return [ Value.String "dune" ]
      | Os Os -> sys_poll_var (fun { os; _ } -> os)
      | Os Os_version -> sys_poll_var (fun { os_version; _ } -> os_version)
      | Os Os_distribution -> sys_poll_var (fun { os_distribution; _ } -> os_distribution)
      | Os Os_family -> sys_poll_var (fun { os_family; _ } -> os_family)
      | Sys_ocaml_version ->
        sys_poll_var (fun { sys_ocaml_version; _ } -> sys_ocaml_version)
      | Build -> Memo.return [ Value.Dir paths.source_dir ]
      | Prefix -> Memo.return [ Value.Dir paths.prefix ]
      | User -> Memo.return [ Value.String (Unix.getlogin ()) ]
      | Jobs -> Memo.return [ Value.String (Int.to_string !Clflags.concurrency) ]
      | Arch -> sys_poll_var (fun { arch; _ } -> arch)
      | Group ->
        let group = Unix.getgid () |> Unix.getgrgid in
        Memo.return [ Value.String group.gr_name ]
      | Section_dir section ->
        let roots = Paths.install_roots paths in
        let dir = section_dir_of_root roots section in
        Memo.return [ Value.Dir dir ]
    ;;

    let expand_pkg_macro ~loc (paths : _ Paths.t) deps macro_invocation =
      let* deps = deps in
      let { Package_variable.name = variable_name; scope } =
        match Package_variable.of_macro_invocation ~loc macro_invocation with
        | Ok package_variable -> package_variable
        | Error `Unexpected_macro ->
          Code_error.raise
            "Attempted to treat an unexpected macro invocation as a package variable \
             encoding"
            []
      in
      let variables, paths =
        let package_name =
          match scope with
          | Self -> paths.name
          | Package package_name -> package_name
        in
        match Package.Name.Map.find deps package_name with
        | None -> Package_variable_name.Map.empty, None
        | Some (var, paths) -> var, Some paths
      in
      match Package_variable_name.Map.find variables variable_name with
      | Some v -> Memo.return @@ Ok (Variable.dune_value v)
      | None ->
        let present = Option.is_some paths in
        (* TODO we should be looking it up in all packages now *)
        (match Package_variable_name.to_string variable_name with
         | "pinned" -> Memo.return @@ Ok [ Value.false_ ]
         | "enable" ->
           Memo.return @@ Ok [ Value.String (if present then "enable" else "disable") ]
         | "installed" -> Memo.return @@ Ok [ Value.String (Bool.to_string present) ]
         | _ ->
           (match paths with
            | None -> Memo.return (Error (`Undefined_pkg_var variable_name))
            | Some paths ->
              (match
                 Pform.Var.Pkg.Section.of_string
                   (Package_variable_name.to_string variable_name)
               with
               | None -> Memo.return (Error (`Undefined_pkg_var variable_name))
               | Some section ->
                 let section = dune_section_of_pform section in
                 let install_paths = Paths.install_paths paths in
                 Memo.return @@ Ok [ Value.Dir (Install.Paths.get install_paths section) ])))
    ;;

    let expand_pform
          { name = _
          ; env = _
          ; paths
          ; artifacts = _
          ; context
          ; depends
          ; version = _
          ; depexts = _
          }
          ~source
          (pform : Pform.t)
      : (Value.t list, [ `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t
      =
      let loc = Dune_sexp.Template.Pform.loc source in
      match pform with
      | Var (Pkg var) -> expand_pkg paths var >>| Result.ok
      | Var Context_name ->
        Memo.return (Ok [ Value.String (Context_name.to_string context) ])
      | Var Make ->
        let+ make =
          let path = Env_path.path (Global.env ()) in
          Make_prog.which loc context ~path
        in
        Ok [ Value.Path make ]
      | Macro ({ macro = Pkg | Pkg_self; _ } as macro_invocation) ->
        expand_pkg_macro ~loc paths depends macro_invocation
      | _ -> Expander0.isn't_allowed_in_this_position ~source
    ;;

    let () = Fdecl.set expand_pform_fdecl expand_pform

    let expand_pform_gen t =
      String_expander.Memo.expand ~dir:t.paths.source_dir ~f:(fun ~source pform ->
        expand_pform t ~source pform
        >>| function
        | Ok x -> x
        | Error (`Undefined_pkg_var variable_name) ->
          User_error.raise
            ~loc:(Dune_sexp.Template.Pform.loc source)
            [ Pp.textf
                "Undefined package variable: %s"
                (Package_variable_name.to_string variable_name)
            ])
    ;;

    let slang_expander t sw =
      String_expander.Memo.expand_result_deferred_concat sw ~mode:Many ~f:(expand_pform t)
    ;;

    let eval_blang t blang =
      Slang_expand.eval_blang blang ~dir:t.paths.source_dir ~f:(slang_expander t)
    ;;

    let eval_slangs_located t slangs =
      let slangs =
        List.map slangs ~f:(fun slang ->
          Slang.map_loc slang ~f:Dune_pkg.Lock_dir.loc_in_source_tree)
      in
      Slang_expand.eval_multi_located slangs ~dir:t.paths.source_dir ~f:(slang_expander t)
    ;;

    let filtered_depexts t =
      Memo.List.filter_map t.depexts ~f:(fun (depexts : Depexts.t) ->
        let+ enabled =
          match depexts.enabled_if with
          | `Always -> Memo.return true
          | `Conditional condition -> eval_blang t condition
        in
        if enabled then Some depexts.external_package_names else None)
      >>| List.concat
      >>| List.sort_uniq ~compare:String.compare
    ;;

    let expand_exe_value t value ~loc =
      let+ prog =
        match value with
        | Value.Dir p ->
          User_error.raise
            ~loc
            [ Pp.textf
                "%s is a directory and cannot be used as an executable"
                (Path.to_string_maybe_quoted p)
            ]
        | Path p -> Memo.return @@ Ok p
        | String program ->
          (match Filename.analyze_program_name program with
           | Relative_to_current_dir | Absolute ->
             let dir = t.paths.source_dir in
             Memo.return @@ Ok (Path.relative dir program)
           | In_path ->
             let* artifacts = t.artifacts in
             (match Filename.Map.find artifacts program with
              | Some s -> Memo.return @@ Ok s
              | None ->
                (let path = Global.env () |> Env_path.path in
                 Which.which ~path program)
                >>= (function
                 | Some p -> Memo.return (Ok p)
                 | None ->
                   let+ depexts = filtered_depexts t in
                   let hint =
                     Run_with_path.depexts_hint depexts
                     |> Option.map ~f:(fun pp -> Format.asprintf "%a" Pp.to_fmt pp)
                   in
                   Error
                     (Action.Prog.Not_found.create
                        ?hint
                        ~program
                        ~context:t.context
                        ~loc:(Some loc)
                        ()))))
      in
      Result.map prog ~f:(map_exe t)
    ;;
  end

  let rec expand (action : Dune_lang.Action.t) ~(expander : Expander.t) =
    let dir = expander.paths.source_dir in
    match action with
    | Run args ->
      Expander.eval_slangs_located expander args
      >>= (function
       | [] ->
         let loc =
           let loc = function
             | Slang.Nil -> None
             | Literal sw -> Some (String_with_vars.loc sw)
             | Form (loc, _) -> Some loc
           in
           let start = List.find_map args ~f:loc in
           let stop =
             List.fold_left args ~init:None ~f:(fun last a ->
               match loc a with
               | None -> last
               | Some _ as s -> s)
           in
           Option.both start stop
           |> Option.map ~f:(fun (start, stop) -> Loc.span start stop)
         in
         User_error.raise
           ?loc
           [ Pp.text "\"run\" action must have at least one argument" ]
       | (prog_loc, prog) :: args ->
         let+ exe =
           let prog = Value.Deferred_concat.force prog ~dir in
           Expander.expand_exe_value expander prog ~loc:prog_loc
         and+ depexts = Expander.filtered_depexts expander in
         let args =
           Array.Immutable.of_list_map args ~f:(fun (_loc, arg) ->
             Value.Deferred_concat.parts arg
             |> Array.Immutable.of_list_map ~f:(fun (arg : Value.t) ->
               match arg with
               | String s -> Run_with_path.Spec.String s
               | Path p | Dir p -> Path p))
         in
         let ocamlfind_destdir = (Lazy.force expander.paths.install_roots).lib_root in
         Run_with_path.action
           ~depexts
           ~pkg:(expander.name, prog_loc)
           exe
           args
           ~ocamlfind_destdir)
    | Progn t ->
      let+ args = Memo.parallel_map t ~f:(expand ~expander) in
      Action.Progn args
    | System arg ->
      Expander.expand_pform_gen ~mode:Single expander arg
      >>| Value.to_string ~dir
      >>| System.action
    | Patch p ->
      let+ patch =
        Expander.expand_pform_gen ~mode:Single expander p >>| Value.to_path ~dir
      in
      Dune_patch.action ~patch
    | Substitute (src, dst) ->
      let* src =
        Expander.expand_pform_gen ~mode:Single expander src >>| Value.to_path ~dir
      and* dst =
        Expander.expand_pform_gen ~mode:Single expander dst
        >>| Value.to_path ~dir
        >>| Expander0.as_in_build_dir ~what:"substitute" ~loc:(String_with_vars.loc dst)
      in
      Substitute.action expander ~src ~dst
    | Withenv (updates, action) -> expand_withenv expander updates action
    | When (condition, action) ->
      Expander.eval_blang expander condition
      >>= (function
       | true -> expand action ~expander
       | false -> Memo.return (Action.progn []))
    | Write_file (path_sw, perm, contents_sw) ->
      let+ path =
        Expander.expand_pform_gen ~mode:Single expander path_sw
        >>| Value.to_path ~dir
        >>| Expander0.as_in_build_dir
              ~what:"write-file"
              ~loc:(String_with_vars.loc path_sw)
      and+ contents =
        Expander.expand_pform_gen ~mode:Single expander contents_sw
        >>| Value.to_string ~dir
      in
      Action.Write_file (path, perm, contents)
    | _ ->
      Code_error.raise
        "Pkg_rules.action_expander.expand: unsupported action"
        [ "action", Dune_lang.Action.to_dyn action ]

  and expand_withenv (expander : Expander.t) updates action =
    let* env, updates =
      let dir = expander.paths.source_dir in
      Memo.List.fold_left
        ~init:(expander.env, [])
        updates
        ~f:(fun (env, updates) ({ Env_update.op = _; var; value } as update) ->
          let+ value =
            let+ value =
              let expander = { expander with env } in
              Expander.expand_pform_gen expander value ~mode:Single
            in
            Value.to_string ~dir value
          in
          let env = Env_update.set env { update with value } in
          let update =
            let value =
              match Env.Map.find env var with
              | Some v -> Value_list_env.string_of_env_values v
              | None ->
                (* TODO *)
                ""
            in
            var, value
          in
          env, update :: updates)
    in
    let+ action =
      let expander = { expander with env } in
      expand action ~expander
    in
    List.fold_left updates ~init:action ~f:(fun action (k, v) ->
      Action.Setenv (k, v, action))
  ;;

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
        let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
        Action_builder.evaluate_and_collect_facts cookie
        |> Memo.map ~f:(fun ((cookie : Install_cookie.t), _) -> pkg, cookie))
      |> Memo.map ~f:(fun (cookies : (Pkg.t * Install_cookie.t) list) ->
        List.fold_left
          cookies
          ~init:empty
          ~f:(fun { binaries; dep_info } ((pkg : Pkg.t), (cookie : Install_cookie.t)) ->
            let binaries =
              Section.Map.Multi.find cookie.files Bin
              |> List.fold_left ~init:binaries ~f:(fun acc bin ->
                Filename.Map.set acc (Path.basename bin) bin)
            in
            let dep_info =
              let variables =
                Package_variable_name.Map.superpose
                  (Package_variable_name.Map.of_list_exn cookie.variables)
                  (Pkg_info.variables pkg.info)
              in
              Package.Name.Map.add_exn dep_info pkg.info.name (variables, pkg.paths)
            in
            { binaries; dep_info }))
    ;;
  end

  let expander context (pkg : Pkg.t) =
    let closure =
      Memo.lazy_
        ~human_readable_description:(fun () ->
          Pp.textf
            "Computing closure for package %S"
            (Package.Name.to_string pkg.info.name))
        (fun () -> Pkg.deps_closure pkg |> Artifacts_and_deps.of_closure)
    in
    let env = Pkg.exported_value_env pkg in
    let depends =
      Memo.Lazy.map closure ~f:(fun { Artifacts_and_deps.dep_info; _ } ->
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
    ; depends
    ; depexts = pkg.depexts
    ; version = pkg.info.version
    ; env
    }
  ;;

  let sandbox = Sandbox_mode.Set.singleton Sandbox_mode.copy

  let rec action_contains_run action =
    match (action : Dune_lang.Action.t) with
    | Run _ -> true
    | Progn actions -> actions |> List.find ~f:action_contains_run |> Option.is_some
    | When (_, action) -> action_contains_run action
    | Withenv (_, action) -> action_contains_run action
    | _ -> false
  ;;

  let expand context (pkg : Pkg.t) action =
    let depend_on_dune =
      match action_contains_run action with
      | false -> Action_builder.return ()
      | true ->
        Path.External.of_string Sys.executable_name
        |> Path.external_
        |> Action_builder.path
    in
    let+ action =
      let expander = expander context pkg in
      expand action ~expander >>| Action.chdir pkg.paths.source_dir
    in
    (* TODO copying is needed for build systems that aren't dune and those
       with an explicit install step *)
    let open Action_builder.O in
    depend_on_dune
    >>> (Action.Full.make ~sandbox action |> Action_builder.return)
    |> Action_builder.with_no_targets
  ;;

  let dune_exe context =
    Which.which ~path:(Env_path.path Env.initial) "dune"
    >>| function
    | Some s -> Ok s
    | None -> Error (Action.Prog.Not_found.create ~loc:None ~context ~program:"dune" ())
  ;;

  let build_command context (pkg : Pkg.t) =
    Option.map pkg.build_command ~f:(function
      | Action action -> expand context pkg action
      | Dune ->
        (* CR-someday rgrinberg: respect [dune subst] settings. *)
        Command.run_dyn_prog
          (Action_builder.of_memo (dune_exe context))
          ~dir:pkg.paths.source_dir
          [ A "build"; A "-p"; A (Package.Name.to_string pkg.info.name) ]
        |> Memo.return)
  ;;

  let install_command context (pkg : Pkg.t) =
    Option.map pkg.install_command ~f:(fun action -> expand context pkg action)
  ;;

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let+ value =
      let+ value = Expander.expand_pform_gen expander env.value ~mode:Single in
      value |> Value.to_string ~dir:expander.paths.source_dir
    in
    { env with value }
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
          let deps =
            Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform pkg.depends ~platform
            |> Option.value ~default:[]
            |> List.filter_map
                 ~f:(fun { Dune_pkg.Lock_dir.Dependency.name; loc = dep_loc } ->
                   if Package.Name.Set.mem system_provided name
                   then None
                   else (
                     let dep_pkg = Package.Name.Map.find_exn pkgs_by_name name in
                     let dep_entry = compute_entry dep_pkg ~seen_set ~seen_list in
                     Some { dep_pkg; dep_loc; dep_pkg_digest = dep_entry.pkg_digest }))
          in
          let pkg_digest =
            Pkg_digest.create
              pkg
              (List.map deps ~f:(fun { dep_pkg_digest; _ } -> dep_pkg_digest))
          in
          { pkg; deps; pkg_digest })
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
          ({ pkg = pkg_a; deps = deps_a; pkg_digest = _ } as entry)
          { pkg = pkg_b; deps = deps_b; pkg_digest = _ }
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
      Memo.lazy_ (fun () ->
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
      Memo.lazy_ (fun () ->
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
        let* lock_dir =
          Package_universe.lock_dir_path package_universe >>| Option.value_exn
        in
        let+ files_dir =
          let module Pkg = Dune_pkg.Lock_dir.Pkg in
          (* TODO(steve): simplify this once portable lockdirs become the
             default. This logic currently handles both the cases where
             lockdirs are non-portable (the files dir won't have a version
             number in its name) and the case where lockdirs are portable (the
             solution may have multiple versions of the same package
             necessitating version numbers in files dirs to prevent
             collisions). *)
          let path_with_version =
            Pkg.source_files_dir info.name (Some info.version) ~lock_dir
          in
          let* path_with_version_exists =
            Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir path_with_version)
          in
          match path_with_version_exists with
          | true ->
            Memo.return @@ Some (Pkg.files_dir info.name (Some info.version) ~lock_dir)
          | false ->
            let path_without_version = Pkg.source_files_dir info.name None ~lock_dir in
            let+ path_without_version_exists =
              Fs_memo.dir_exists
                (Path.Outside_build_dir.In_source_dir path_without_version)
            in
            (match path_without_version_exists with
             | true -> Some (Pkg.files_dir info.name None ~lock_dir)
             | false -> None)
        in
        files_dir
        |> Option.map ~f:(fun (p : Path.t) ->
          match p with
          | External e ->
            let source_path = Dune_pkg.Pkg_workspace.dev_tool_path_to_source_dir e in
            (match Path.Source.explode source_path with
             | [ "_build"; ".dev-tools.locks"; dev_tool; files_dir ] ->
               Path.Build.L.relative
                 Private_context.t.build_dir
                 [ "default"; ".dev-tool-locks"; dev_tool; files_dir ]
             | components ->
               Code_error.raise
                 "Package files directory is external source directory, this is \
                  unsupported"
                 [ "external", Path.External.to_dyn e
                 ; "source", Path.Source.to_dyn source_path
                 ; "components", Dyn.(list string) components
                 ])
          | In_source_tree s ->
            Code_error.raise "Unexpected files_dir path" [ "dir", Path.Source.to_dyn s ]
          | In_build_dir b -> b)
      in
      let id = Pkg.Id.gen () in
      let write_paths =
        Paths.make pkg_digest package_universe ~relative:Path.Build.relative
      in
      let install_command = choose_for_current_platform install_command in
      let install_command = Option.map install_command ~f:relocate in
      let build_command = choose_for_current_platform build_command in
      let build_command = Option.map build_command ~f:relocate_build in
      let paths =
        let paths = Paths.map_path write_paths ~f:Path.build in
        match Pkg_toolchain.is_compiler_and_toolchains_enabled info.name with
        | false -> paths
        | true ->
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
               build and install commands are replaced with no-ops *)
          let prefix = Pkg_toolchain.installation_prefix pkg in
          let install_roots =
            Pkg_toolchain.install_roots ~prefix
            |> Install.Roots.map ~f:Path.outside_build_dir
          in
          { paths with
            prefix = Path.outside_build_dir prefix
          ; install_roots = Lazy.from_val install_roots
          }
      in
      let t =
        { Pkg.id
        ; build_command
        ; install_command
        ; depends
        ; depexts
        ; paths
        ; write_paths
        ; info
        ; files_dir
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
          Pp.textf "- package %s" (Package.Name.to_string t.pkg_digest.name))
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

module Install_action = struct
  (* The install action does the following:

     1. Runs the install action in the lock file (if exists)
     2. Reads the .install file produced by the build command
     3. Discoves all the files produced by 1.
     4. Combines the set of files in 2. and 3. to produce a "cookie" file
  *)

  let installable_sections =
    Section.(Set.diff all (Set.of_list [ Misc; Libexec; Libexec_root ]))
    |> Section.Set.to_list
  ;;

  module Spec = struct
    type ('path, 'target) t =
      { (* location of the install file we must read (if produced) *)
        install_file : 'path
      ; (* location of the variables we must read (if produced) *)
        config_file : 'path
      ; (* where we are supposed to put the installed artifacts *)
        target_dir : 'target
      ; (* if the package's installation prefix is outside the build
           dir, it's stored here and will be used instead of [target_dir]
           as the location of insntalled artifacts *)
        prefix_outside_build_dir : Path.Outside_build_dir.t option
      ; (* does the package have its own install command? *)
        install_action : [ `Has_install_action | `No_install_action ]
      ; package : Package.Name.t
      }

    let name = "install-file-run"
    let version = 1

    let bimap
          ({ install_file
           ; config_file
           ; target_dir
           ; prefix_outside_build_dir = _
           ; install_action = _
           ; package = _
           } as t)
          f
          g
      =
      { t with
        install_file = f install_file
      ; config_file = f config_file
      ; target_dir = g target_dir
      }
    ;;

    let is_useful_to ~memoize = memoize

    let encode
          { install_file
          ; config_file
          ; target_dir
          ; prefix_outside_build_dir
          ; install_action
          ; package
          }
          path
          target
      : Sexp.t
      =
      List
        [ path install_file
        ; path config_file
        ; target target_dir
        ; (match
             Option.map
               prefix_outside_build_dir
               ~f:Path.Outside_build_dir.to_string_maybe_quoted
           with
           | None -> List []
           | Some s -> List [ Atom s ])
        ; Atom (Package.Name.to_string package)
        ; Atom
            (match install_action with
             | `Has_install_action -> "has_install_action"
             | `No_install_action -> "no_install_action")
        ]
    ;;

    let prepare_copy ~install_file ~target_dir entry =
      let dst =
        let paths =
          let package =
            Path.basename install_file
            |> Filename.remove_extension
            |> Package.Name.of_string
          in
          let roots =
            Path.build target_dir
            |> Install.Roots.opam_from_prefix ~relative:Path.relative
          in
          Install.Paths.make ~relative:Path.relative ~package ~roots
        in
        Install.Entry.relative_installed_path entry ~paths
      in
      Path.mkdir_p (Path.parent_exn dst);
      dst
    ;;

    let readdir path =
      match Path.Untracked.readdir_unsorted_with_kinds path with
      | Error _ -> [], []
      | Ok listing ->
        List.partition_map listing ~f:(fun (basename, kind) ->
          let path = Path.relative path basename in
          match kind with
          | S_DIR -> Right path
          | _ -> Left path)
    ;;

    let rec collect paths acc =
      match paths with
      | [] -> acc
      | path :: paths ->
        let files, dirs = readdir path in
        let acc = List.rev_append files acc in
        collect (List.rev_append dirs paths) acc
    ;;

    let skip path skip =
      List.iter skip ~f:(fun s -> assert (Path.equal path (Path.parent_exn s)));
      let files, dirs = readdir path in
      let dirs =
        List.filter_map dirs ~f:(fun path ->
          if List.mem skip path ~equal:Path.equal then None else Some path)
      in
      files, dirs
    ;;

    let maybe_drop_sandbox_dir path =
      match Path.extract_build_context_dir_maybe_sandboxed path with
      | None -> path
      | Some (sandbox, source) ->
        let ctx =
          let name = Path.basename sandbox in
          Path.relative (Path.build Path.Build.root) name
        in
        Path.append_source ctx source
    ;;

    let section_map_of_dir install_paths =
      (* reverse engineer the installed artifacts from running the install
         action by looking at the file system post running the action and
         taking educated guesses about which section each file belongs to *)
      let get = Install.Paths.get install_paths in
      List.concat_map installable_sections ~f:(fun section ->
        let path = get section in
        let acc, dirs =
          match section with
          | Lib_root -> skip path [ get Toplevel; get Stublibs; get Lib ]
          | Share_root -> skip path [ get Share ]
          | _ -> [], [ path ]
        in
        collect dirs acc
        |> List.rev_map ~f:(fun file ->
          let section =
            match
              match section with
              | Lib_root -> Some Section.Libexec_root
              | Lib -> Some Libexec
              | _ -> None
            with
            | None -> section
            | Some section' ->
              let perm = (Unix.stat (Path.to_string file)).st_perm in
              if Path.Permissions.(test execute perm) then section' else section
          in
          section, maybe_drop_sandbox_dir file))
      |> Section.Map.of_list_multi
    ;;

    let maybe_set_executable section dst =
      match Section.should_set_executable_bit section with
      | false -> ()
      | true ->
        let dst = Path.to_string dst in
        let permission =
          let perm = (Unix.stat dst).st_perm in
          Path.Permissions.(add execute) perm
        in
        Unix.chmod dst permission
    ;;

    let read_variables config_file =
      match Path.Untracked.exists config_file with
      | false -> []
      | true ->
        let config =
          let filename = Path.to_string config_file in
          match
            Io.read_file config_file
            |> OpamFile.Dot_config.read_from_string
                 ~filename:(OpamFile.make (OpamFilename.of_string filename))
          with
          | s -> s
          | exception OpamPp.Bad_format (pos, message) ->
            let loc =
              Option.map
                pos
                ~f:(fun { OpamParserTypes.FullPos.filename = _; start; stop } ->
                  let file_contents = Io.read_file config_file in
                  let bols = ref [ 0 ] in
                  String.iteri file_contents ~f:(fun i ch ->
                    if ch = '\n' then bols := (i + 1) :: !bols);
                  let bols = Array.of_list (List.rev !bols) in
                  let make_pos (line, column) =
                    let pos_bol = bols.(line - 1) in
                    { Lexing.pos_fname = filename
                    ; pos_lnum = line
                    ; pos_bol
                    ; pos_cnum = pos_bol + column
                    }
                  in
                  let start = make_pos start in
                  let stop = make_pos stop in
                  Loc.create ~start ~stop)
            in
            let message_with_loc =
              (* The location is inlined b/c the original config file is going
                 to be deleted, so we don't be able to fetch the part of the
                 file that's bad *)
              let open Pp.O in
              let error = Pp.textf "Error parsing %s" (Path.basename config_file) in
              match loc with
              | None -> error
              | Some loc ->
                (Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> User_message.Style.Loc))
                ++ error
            in
            User_error.raise
              [ message_with_loc; Pp.seq (Pp.text "Reason: ") (Pp.text message) ]
        in
        OpamFile.Dot_config.bindings config
        |> List.map ~f:(fun (name, value) -> Package_variable_name.of_opam name, value)
    ;;

    let install_entry ~src ~install_file ~target_dir (entry : Path.t Install.Entry.t) =
      match Path.Untracked.exists src, entry.optional with
      | false, true -> None
      | false, false ->
        User_error.raise
          (* TODO loc *)
          [ Pp.textf
              "entry %s in %s does not exist"
              (Path.to_string_maybe_quoted src)
              (Path.to_string install_file)
          ]
      | true, _ ->
        let dst = prepare_copy ~install_file ~target_dir entry in
        (let src =
           match Path.to_string src |> Unix.readlink with
           | exception Unix.Unix_error (_, _, _) -> src
           | link ->
             Path.external_
               (let base = Path.parent_exn src in
                Filename.concat (Path.to_absolute_filename base) link
                |> Path.External.of_string)
         in
         Io.portable_hardlink ~src ~dst);
        maybe_set_executable entry.section dst;
        Some (entry.section, dst)
    ;;

    let action
          { package
          ; install_file
          ; config_file
          ; target_dir
          ; prefix_outside_build_dir
          ; install_action
          }
          ~ectx:_
          ~eenv:_
      =
      let open Fiber.O in
      let* () = Fiber.return () in
      let* files =
        let from_install_action =
          let target_dir =
            (* If the package used a prefix that was outside the build
               directory (as is the case with toolchains), parse the
               installed sections from that location. Otherwise parse the
               installed sections from the package's location within the
               build directory. *)
            match prefix_outside_build_dir with
            | Some prefix_outside_build_dir ->
              Path.outside_build_dir prefix_outside_build_dir
            | None -> Path.build target_dir
          in
          match install_action with
          | `No_install_action -> Section.Map.empty
          | `Has_install_action ->
            let install_paths =
              Paths.of_root
                package
                ~root:(Path.parent_exn target_dir)
                ~relative:Path.relative
              |> Paths.install_paths
            in
            section_map_of_dir install_paths
        in
        let+ from_install_file =
          (* Read all the artifacts from the .install file produced by
             the build command. This is the happy path where we don't guess
             anything. *)
          Async.async (fun () -> Path.Untracked.exists install_file)
          >>= function
          | false -> Fiber.return Section.Map.empty
          | true ->
            let* map =
              let install_entries =
                let dir = Path.parent_exn install_file in
                Install.Entry.load_install_file install_file (fun local ->
                  Path.append_local dir local)
              in
              let by_src =
                List.rev_map install_entries ~f:(fun (entry : _ Install.Entry.t) ->
                  entry.src, entry)
                |> Path.Map.of_list_multi
              in
              let+ install_entries =
                Path.Map.to_list_map by_src ~f:(fun src entries ->
                  List.map entries ~f:(fun entry -> src, entry))
                |> List.concat
                |> Fiber.parallel_map ~f:(fun (src, entry) ->
                  Async.async (fun () ->
                    install_entry ~src ~install_file ~target_dir entry))
                >>| List.filter_opt
              in
              List.rev_map install_entries ~f:(fun (section, file) ->
                let file = maybe_drop_sandbox_dir file in
                section, file)
              |> Section.Map.of_list_multi
            in
            let+ () =
              Async.async (fun () -> Fpath.unlink_exn (Path.to_string install_file))
            in
            map
        in
        (* Combine the artifacts declared in the .install, and the ones we discovered
           by runing the install action *)
        (* TODO we should make sure that overwrites aren't allowed *)
        Section.Map.union from_install_action from_install_file ~f:(fun _ x y ->
          Some (x @ y))
        |> Section.Map.map ~f:(List.sort ~compare:Path.compare)
      in
      let* cookies =
        let+ variables = Async.async (fun () -> read_variables config_file) in
        { Install_cookie.Gen.files; variables }
      in
      (* Produce the cookie file in the standard path *)
      let cookie_file = Path.build @@ Paths.install_cookie' target_dir in
      Async.async (fun () ->
        cookie_file |> Path.parent_exn |> Path.mkdir_p;
        Install_cookie.dump cookie_file cookies)
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action (p : Path.Build.t Paths.t) install_action ~prefix_outside_build_dir =
    A.action
      { Spec.install_file = Path.build @@ Paths.install_file p
      ; config_file = Path.build @@ Paths.config_file p
      ; target_dir = p.target_dir
      ; prefix_outside_build_dir
      ; install_action
      ; package = p.name
      }
  ;;
end

let add_env env action =
  Action_builder.With_targets.map action ~f:(Action.Full.add_env env)
;;

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
           let source_root = Path.external_ source_root in
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
                  extracted. This logic is implemented in the "source-fetch"
                  action spec in [Fetch_rules]. *)
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
      let p = P.relative p file_name in
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

let build_rule context_name ~source_deps (pkg : Pkg.t) =
  let+ build_action =
    let+ copy_action, build_action, install_action =
      let+ copy_action =
        let+ copy_action =
          let+ () = Memo.return () in
          let open Action_builder.O in
          [ Action_builder.with_no_targets
            @@ ((match pkg.files_dir with
                 | Some files_dir -> Action_builder.path (Path.build files_dir)
                 | None -> Action_builder.return ())
                >>> Action_builder.of_memo
                      (Memo.of_thunk (fun () ->
                         match pkg.files_dir with
                         | None -> Memo.return (Path.Set.empty, Dep.Set.empty)
                         | Some files_dir ->
                           let deps, source_deps = files files_dir in
                           Memo.return (source_deps, deps)))
                |> Action_builder.dyn_deps
                >>= fun source_deps ->
                Path.Set.to_list_map source_deps ~f:(fun src ->
                  let dst =
                    let prefix = pkg.files_dir |> Option.value_exn |> Path.build in
                    let local_path = Path.drop_prefix_exn src ~prefix in
                    Path.Build.append_local pkg.write_paths.source_dir local_path
                  in
                  Action.progn
                    [ Action.mkdir (Path.Build.parent_exn dst); Action.copy src dst ])
                |> Action.concurrent
                |> Action.Full.make
                |> Action_builder.return)
          ]
        in
        copy_action
        @ List.map pkg.info.extra_sources ~f:(fun (local, _) ->
          (* If the package has extra sources, they will be
             initially stored in the extra_sources directory for that
             package. Prior to building, the contents of
             extra_sources must be copied into the package's source
             directory. *)
          let src = Paths.extra_source pkg.paths local in
          let dst = Path.Build.append_local pkg.write_paths.source_dir local in
          Action.progn
            [ (* If the package has no source directory (some
                 low-level packages are exclusively made up of extra
                 sources), the source directory is first created. *)
              Action.mkdir pkg.write_paths.source_dir
            ; (* It's possible for some extra sources to already be at
                 the destination. If these files are write-protected
                 then the copy action will fail if we don't first remove
                 them. *)
              Action.remove_tree dst
            ; Action.copy src dst
            ]
          |> Action.Full.make
          |> Action_builder.With_targets.return)
      and+ build_action =
        match Action_expander.build_command context_name pkg with
        | None -> Memo.return []
        | Some build_command -> build_command >>| List.singleton
      and+ install_action =
        match Action_expander.install_command context_name pkg with
        | None -> Memo.return []
        | Some install_action ->
          let+ install_action = install_action in
          let mkdir_install_dirs =
            let install_paths = Paths.install_paths pkg.write_paths in
            Install_action.installable_sections
            |> List.rev_map ~f:(fun section ->
              Install.Paths.get install_paths section |> Action.mkdir)
            |> Action.progn
            |> Action.Full.make
            |> Action_builder.With_targets.return
          in
          [ mkdir_install_dirs; install_action ]
      in
      copy_action, build_action, install_action
    in
    let install_file_action =
      let prefix_outside_build_dir = Path.as_outside_build_dir pkg.paths.prefix in
      Install_action.action
        pkg.write_paths
        (match Action_expander.install_command context_name pkg with
         | None -> `No_install_action
         | Some _ -> `Has_install_action)
        ~prefix_outside_build_dir
      |> Action.Full.make
      |> Action_builder.return
      |> Action_builder.with_no_targets
    in
    (* Action to print a "Building" message for the package if its
       target directory is not yet created. *)
    let progress_building =
      Pkg_build_progress.progress_action pkg.info.name pkg.info.version `Building
      |> Action.Full.make
      |> Action_builder.return
      |> Action_builder.with_no_targets
    in
    [ copy_action
    ; [ progress_building ]
    ; build_action
    ; install_action
    ; [ install_file_action ]
    ]
    |> List.concat
    |> Action_builder.progn
  in
  let deps = Dep.Set.union source_deps (Pkg.package_deps pkg) in
  let open Action_builder.With_targets.O in
  Action_builder.deps deps
  |> Action_builder.with_no_targets
  (* TODO should we add env deps on these? *)
  >>> add_env (Pkg.exported_env pkg) build_action
  |> Action_builder.With_targets.add_directories
       ~directory_targets:[ pkg.write_paths.target_dir ]
;;

let gen_rules context_name (pkg : Pkg.t) =
  let* source_deps, copy_rules = source_rules pkg in
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
    let* pkg_digests =
      Action_builder.of_memo
        (let open Memo.O in
         let+ db = DB.of_ctx ctx_name ~allow_sharing:true in
         Pkg_digest.Map.values db.pkg_digest_table
         |> List.map ~f:(fun { DB.Pkg_table.pkg_digest; _ } -> pkg_digest))
    in
    List.map pkg_digests ~f:(fun pkg_digest ->
      Paths.make ~relative:Path.Build.relative pkg_digest (Dependencies ctx_name)
      |> Paths.target_dir
      |> Path.build)
    |> Action_builder.paths
  in
  fun ~dir ctx_name ->
    let rule =
      (* We only need to build when the build_dir is the root of the context *)
      match
        let build_dir = Context_name.build_dir ctx_name in
        Path.Build.equal dir build_dir
      with
      | false -> Memo.return Rules.empty
      | true ->
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
    Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
    |> Gen_rules.rules_here
;;

let setup_package_rules db ~package_universe ~dir ~pkg_digest : Gen_rules.result Memo.t =
  let* pkg = Resolve.resolve db Loc.none pkg_digest package_universe in
  let paths = Paths.make pkg.pkg_digest package_universe ~relative:Path.Build.relative in
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
  | true, [ ".dev-tool" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | _, [ ".pkg" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
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
    Memo.return @@ Gen_rules.redirect_to_parent Gen_rules.Rules.empty
  | true, ".dev-tool" :: _ :: _ :: _ ->
    Memo.return @@ Gen_rules.redirect_to_parent Gen_rules.Rules.empty
  | is_default, [] ->
    let sub_dirs = ".pkg" :: (if is_default then [ ".dev-tool" ] else []) in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;

let resolve_pkg_dep context (loc, package_name) =
  let* db, pkg_digest = DB.of_project_pkg context package_name in
  Resolve.resolve db loc pkg_digest (Dependencies context)
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

let which context =
  let artifacts_and_deps =
    Memo.lazy_
      ~human_readable_description:(fun () ->
        Pp.textf
          "Loading all binaries in the lock directory for %S"
          (Context_name.to_string context))
      (fun () ->
         let+ { binaries; dep_info = _ } =
           all_project_deps context >>= Action_expander.Artifacts_and_deps.of_closure
         in
         binaries)
  in
  Staged.stage (fun program ->
    let+ artifacts = Memo.Lazy.force artifacts_and_deps in
    Filename.Map.find artifacts program)
;;

let ocamlpath universe =
  let+ all_project_deps = all_deps universe in
  let env = Pkg.build_env_of_deps all_project_deps in
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

let find_package ctx pkg =
  lock_dir_active ctx
  >>= function
  | false -> Memo.return None
  | true ->
    let+ pkg = resolve_pkg_dep ctx (Loc.none, pkg) in
    Some
      (let open Action_builder.O in
       let+ _cookie = (Pkg_installed.of_paths pkg.paths).cookie in
       ())
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
