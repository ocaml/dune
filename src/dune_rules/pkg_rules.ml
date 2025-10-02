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
  (* A type of group of packages that are co-installed. Different
     package universes are unaware of each other. For example the
     dependencies of the project and the dependencies of one of the dev
     tools don't need to be mutually co-installable as they are in
     different universes. *)
  type t =
    | Project_dependencies of Context_name.t
    | Dev_tool of Dune_pkg.Dev_tool.t

  let equal a b =
    match a, b with
    | Project_dependencies a, Project_dependencies b -> Context_name.equal a b
    | Dev_tool a, Dev_tool b -> Dune_pkg.Dev_tool.equal a b
    | _ -> false
  ;;

  let hash t =
    match t with
    | Project_dependencies context_name ->
      Tuple.T2.hash Int.hash Context_name.hash (0, context_name)
    | Dev_tool dev_tool -> Tuple.T2.hash Int.hash Dune_pkg.Dev_tool.hash (1, dev_tool)
  ;;

  let to_dyn = Dyn.opaque

  let context_name = function
    | Project_dependencies context_name -> context_name
    | Dev_tool _ ->
      (* Dev tools can only be built in the default context. *)
      Context_name.default
  ;;

  let lock_dir t =
    match t with
    | Project_dependencies ctx -> Lock_dir.get_exn ctx
    | Dev_tool dev_tool -> Lock_dir.of_dev_tool dev_tool
  ;;

  let lock_dir_path t =
    match t with
    | Project_dependencies ctx -> Lock_dir.get_path ctx
    | Dev_tool dev_tool ->
      Memo.return (Some (Dune_pkg.Lock_dir.dev_tool_lock_dir_path dev_tool))
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

  let make package_universe name =
    let universe_root =
      match (package_universe : Package_universe.t) with
      | Dev_tool dev_tool -> Pkg_dev_tool.universe_install_path dev_tool
      | Project_dependencies _ ->
        let build_dir =
          Path.Build.relative
            Private_context.t.build_dir
            (Context_name.to_string (Package_universe.context_name package_universe))
        in
        Path.Build.relative build_dir ".pkg"
    in
    let root = Path.Build.relative universe_root (Package.Name.to_string name) in
    of_root name ~root
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

  let parse_strings s = Bin.parse s |> List.map ~f:(fun s -> Value.String s)
  let of_env env : t = Env.to_map env |> Env.Map.map ~f:parse_strings

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
    ; files_dir : Path.Build.t
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
    Value_list_env.extend_concat_path (Value_list_env.of_env (Global.env ())) package_env
  ;;

  let exported_env t = Value_list_env.to_env @@ exported_value_env t
end

module Pkg_installed = struct
  type t = { cookie : Install_cookie.t Action_builder.t }

  let of_paths (paths : _ Paths.t) =
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
        , expander.version
        , expander.context
        , Env.Map.to_list expander.env |> Digest.generic |> Digest.to_string_raw )
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

  let expand context (pkg : Pkg.t) action =
    let+ action =
      let expander = expander context pkg in
      expand action ~expander >>| Action.chdir pkg.paths.source_dir
    in
    (* TODO copying is needed for build systems that aren't dune and those
       with an explicit install step *)
    Action.Full.make ~sandbox action
    |> Action_builder.return
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
  type t =
    { all : Lock_dir.Pkg.t Package.Name.Map.t
    ; system_provided : Package.Name.Set.t
    }

  let equal t t2 =
    phys_equal t t2
    ||
    let { all; system_provided } = t2 in
    Package.Name.Map.equal ~equal:Lock_dir.Pkg.equal t.all all
    && Package.Name.Set.equal t.system_provided system_provided
  ;;

  let hash = `Do_not_hash
  let _ = hash
  (* Because t is large, hashing is expensive, so much so that hashing the db in Input.t
     below slowed down the dune call in the test repo described in #12248 from 1s to
     2s. *)

  let get =
    let memo =
      Memo.create
        "DB.get"
        ~input:(module Package_universe)
        (fun package_universe ->
           let dune = Package.Name.Set.singleton (Package.Name.of_string "dune") in
           let+ lock_dir = Package_universe.lock_dir package_universe
           and+ solver_env = Lock_dir.Sys_vars.solver_env () in
           let all =
             Dune_pkg.Lock_dir.packages_on_platform lock_dir ~platform:solver_env
           in
           { all; system_provided = dune })
    in
    fun packages_universe -> Memo.exec memo packages_universe
  ;;
end

module rec Resolve : sig
  val resolve
    :  DB.t
    -> Loc.t * Package.Name.t
    -> Package_universe.t
    -> [ `Inside_lock_dir of Pkg.t | `System_provided ] Memo.t
end = struct
  open Resolve

  module Input = struct
    type t =
      { db : DB.t
      ; package : Package.Name.t
      ; universe : Package_universe.t
      }

    let equal { db; package; universe } t =
      Package.Name.equal package t.package
      && Package_universe.equal universe t.universe
      && DB.equal db t.db
    ;;

    let hash { db; package; universe } =
      let _ = db in
      Tuple.T2.hash Package.Name.hash Package_universe.hash (package, universe)
    ;;

    let to_dyn = Dyn.opaque
  end

  let resolve_impl { Input.db; package = name; universe = package_universe } =
    match Package.Name.Map.find db.all name with
    | None -> Memo.return None
    | Some
        ({ Lock_dir.Pkg.build_command
         ; install_command
         ; depends
         ; info
         ; exported_env
         ; depexts
         ; enabled_on_platforms = _
         } as pkg) ->
      assert (Package.Name.equal name info.name);
      let* platform = Lock_dir.Sys_vars.solver_env () in
      let choose_for_current_platform field =
        Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform field ~platform
      in
      let depends = choose_for_current_platform depends |> Option.value ~default:[] in
      let* depends =
        Memo.parallel_map depends ~f:(fun dependency ->
          resolve db (dependency.loc, dependency.name) package_universe
          >>| function
          | `Inside_lock_dir pkg -> Some pkg
          | `System_provided -> None)
        >>| List.filter_opt
      and+ files_dir =
        let* lock_dir =
          Package_universe.lock_dir_path package_universe >>| Option.value_exn
        in
        let+ files_dir =
          (* TODO(steve): simplify this once portable lockdirs become the
             default. This logic currently handles both the cases where
             lockdirs are non-portable (the files dir won't have a version
             number in its name) and the case where lockdirs are portable (the
             solution may have multiple versions of the same package
             necessitating version numbers in files dirs to prevent
             collisions). *)
          let path_without_version =
            Dune_pkg.Lock_dir.Pkg.files_dir info.name None ~lock_dir
          in
          let path_with_version =
            Dune_pkg.Lock_dir.Pkg.files_dir info.name (Some info.version) ~lock_dir
          in
          let+ path_with_version_exists =
            path_with_version |> Path.as_outside_build_dir_exn |> Fs_memo.dir_exists
          in
          if path_with_version_exists then path_with_version else path_without_version
        in
        let build_path =
          Context_name.build_dir (Package_universe.context_name package_universe)
        in
        match files_dir with
        | External e ->
          Code_error.raise
            "Package files directory is external source directory, this is unsupported"
            [ "dir", Path.External.to_dyn e ]
        | In_source_tree s -> Path.Build.append_source build_path s
        | In_build_dir s -> Path.Build.append build_path s
      in
      let id = Pkg.Id.gen () in
      let write_paths = Paths.make package_universe name ~relative:Path.Build.relative in
      let install_command = choose_for_current_platform install_command in
      let build_command = choose_for_current_platform build_command in
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
          Pp.textf "- package %s" (Package.Name.to_string t.package))
        resolve_impl
    in
    fun (db : DB.t) (loc, name) package_universe ->
      if Package.Name.Set.mem db.system_provided name
      then Memo.return `System_provided
      else
        Memo.exec memo { db; package = name; universe = package_universe }
        >>| function
        | Some s -> `Inside_lock_dir s
        | None ->
          User_error.raise
            ~loc
            [ Pp.textf "Unknown package %S" (Package.Name.to_string name) ]
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
              let perm = (Path.Untracked.stat_exn file).st_perm in
              if Path.Permissions.(test execute perm) then section' else section
          in
          section, maybe_drop_sandbox_dir file))
      |> Section.Map.of_list_multi
    ;;

    let maybe_set_executable section dst =
      match Section.should_set_executable_bit section with
      | false -> ()
      | true ->
        let permission =
          let perm = (Path.Untracked.stat_exn dst).st_perm in
          Path.Permissions.(add execute) perm
        in
        Path.chmod dst ~mode:permission
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
            let+ () = Async.async (fun () -> Path.unlink_exn install_file) in
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

let build_rule context_name ~source_deps (pkg : Pkg.t) =
  let+ build_action =
    let+ copy_action, build_action, install_action =
      let+ copy_action =
        let+ copy_action =
          Fs_memo.dir_exists
            (In_source_dir (Path.Build.drop_build_context_exn pkg.files_dir))
          >>= function
          | false -> Memo.return []
          | true ->
            let+ deps, source_deps = Source_deps.files (Path.build pkg.files_dir) in
            let open Action_builder.O in
            [ Action_builder.with_no_targets
              @@ (Action_builder.deps deps
                  >>> (Path.Set.to_list_map source_deps ~f:(fun src ->
                         let dst =
                           let local_path =
                             Path.drop_prefix_exn src ~prefix:(Path.build pkg.files_dir)
                           in
                           Path.Build.append_local pkg.write_paths.source_dir local_path
                         in
                         Action.progn
                           [ Action.mkdir (Path.Build.parent_exn dst)
                           ; Action.copy src dst
                           ])
                       |> Action.concurrent
                       |> Action.Full.make
                       |> Action_builder.return))
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
    let project_deps : Package_universe.t = Project_dependencies ctx_name in
    let* packages =
      Action_builder.of_memo
        (let open Memo.O in
         let+ lock_dir = Package_universe.lock_dir project_deps
         and+ platform = Lock_dir.Sys_vars.solver_env () in
         Dune_pkg.Lock_dir.Packages.pkgs_on_platform_by_name lock_dir.packages ~platform)
    in
    Dune_lang.Package_name.Map.keys packages
    |> List.map ~f:(fun pkg ->
      Paths.make ~relative:Path.Build.relative project_deps pkg
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

let setup_package_rules ~package_universe ~dir ~pkg_name : Gen_rules.result Memo.t =
  let name = User_error.ok_exn (Package.Name.of_string_user_error (Loc.none, pkg_name)) in
  let* db = DB.get package_universe in
  let* pkg =
    Resolve.resolve db (Loc.none, name) package_universe
    >>| function
    | `Inside_lock_dir pkg -> pkg
    | `System_provided ->
      User_error.raise
        (* TODO loc *)
        [ Pp.textf
            "There are no rules for %S because it's set as provided by the system"
            (Package.Name.to_string name)
        ]
  in
  let paths = Paths.make package_universe name ~relative:Path.Build.relative in
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
  | true, [ ".dev-tool"; pkg_name; pkg_dep_name ] ->
    setup_package_rules
      ~package_universe:
        (Dev_tool (Package.Name.of_string pkg_name |> Dune_pkg.Dev_tool.of_package_name))
      ~dir
      ~pkg_name:pkg_dep_name
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
  | _, [ ".pkg"; pkg_name ] ->
    setup_package_rules ~package_universe:(Project_dependencies ctx) ~dir ~pkg_name
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

let db_project context = DB.get (Project_dependencies context)

let resolve_pkg_project context pkg =
  let* db = db_project context in
  Resolve.resolve db pkg (Project_dependencies context)
;;

let ocaml_toolchain context =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf
      "Loading OCaml toolchain from Lock directory for context %S"
      (Context_name.to_string context))
  @@ fun () ->
  (let* lock_dir = Lock_dir.get_exn context in
   match lock_dir.ocaml with
   | None -> Memo.return `System_provided
   | Some ocaml -> resolve_pkg_project context ocaml)
  >>| function
  | `System_provided -> None
  | `Inside_lock_dir pkg ->
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
  let* db = DB.get universe in
  Dune_lang.Package_name.Map.values db.all
  |> Memo.parallel_map ~f:(fun (package : Lock_dir.Pkg.t) ->
    let package = package.info.name in
    Resolve.resolve db (Loc.none, package) universe
    >>| function
    | `Inside_lock_dir pkg -> Some pkg
    | `System_provided -> None)
  >>| List.filter_opt
  >>| Pkg.top_closure
;;

let all_project_deps context = all_deps (Project_dependencies context)

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

let project_ocamlpath context = ocamlpath (Project_dependencies context)
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
  let universe : Package_universe.t = Dev_tool tool in
  let* db = DB.get universe in
  Resolve.resolve db (Loc.none, package_name) universe
  >>| function
  | `System_provided -> assert false
  | `Inside_lock_dir pkg -> Pkg.exported_env pkg
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
    resolve_pkg_project ctx (Loc.none, pkg)
    >>| (function
     | `System_provided -> Action_builder.return ()
     | `Inside_lock_dir pkg ->
       let open Action_builder.O in
       let+ _cookie = (Pkg_installed.of_paths pkg.paths).cookie in
       ())
    >>| Option.some
;;

let all_filtered_depexts context =
  let* all_project_deps = all_project_deps context in
  Memo.List.map all_project_deps ~f:(fun (pkg : Pkg.t) ->
    let expander = Action_expander.expander context pkg in
    Action_expander.Expander.filtered_depexts expander)
  >>| List.concat
  >>| List.sort_uniq ~compare:String.compare
;;
