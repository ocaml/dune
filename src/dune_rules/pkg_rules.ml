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
  module Dune_dep = Dune_dep
end

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  let value_repr =
    Repr.variant
      "package-variable-value"
      [ Repr.case "Bool" Repr.bool ~proj:(function
          | B b -> Some b
          | _ -> None)
      ; Repr.case "String" Repr.string ~proj:(function
          | S s -> Some s
          | _ -> None)
      ; Repr.case "Strings" (Repr.list Repr.string) ~proj:(function
          | L xs -> Some xs
          | _ -> None)
      ]
  ;;

  let repr = Repr.pair Package_variable_name.repr value_repr

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
end

module Package_universe = struct
  (* A context or dev tool owns the targets for every package in its lock. *)
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

  let repr =
    Repr.variant
      "package-universe"
      [ Repr.case
          "Dependencies"
          (Repr.view Repr.string ~to_:Context_name.to_string)
          ~proj:(function
          | Dependencies context -> Some context
          | Dev_tool _ -> None)
      ; Repr.case
          "Dev_tool"
          (Repr.view Package.Name.repr ~to_:Dune_pkg.Dev_tool.package_name)
          ~proj:(function
          | Dev_tool tool -> Some tool
          | Dependencies _ -> None)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr

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

module Backing = Source_tree_file.Backing

let package_of_dir_in_context dir =
  match Path.Local.explode dir |> Filename.L.to_string with
  | root :: name :: _
    when String.equal root (Filename.to_string Filename.pkg_dir_basename) ->
    Package.Name.of_string_opt name
  | _ -> None
;;

let package_of_artifact_path path =
  match Path.Build.split_first_component path with
  | None -> None
  | Some (_, dir) -> package_of_dir_in_context dir
;;

let disable_promotion_in_package ~dir action =
  match package_of_artifact_path dir with
  | None -> action
  | Some _ -> Action.Full.disable_promotion action
;;

let source_location path =
  match package_of_artifact_path path with
  | Some _ -> Path.build path
  | None -> Path.Build.drop_build_context_exn path |> Path.source
;;

module Context_and_package = struct
  type t = Context_name.t * Package.Name.t

  let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn Package.Name.to_dyn
  let equal = Tuple.T2.equal Context_name.equal Package.Name.equal
  let hash = Tuple.T2.hash Context_name.hash Package.Name.hash
end

module Source_input = struct
  type t =
    { files : (Path.t * Path.Local.t) list
    ; directories : Path.Local.t list
    }
end

module Candidate = struct
  type t =
    { lock_pkg : Dune_pkg.Lock_dir.Pkg.t
    ; files_dir : Path.Build.t
    ; artifact_root : Path.Build.t
    }

  let name t = t.lock_pkg.info.name

  let source_loc t =
    match t.lock_pkg.info.source with
    | None -> Loc.none
    | Some source -> fst source.url
  ;;

  let source_root t =
    match t.lock_pkg.info.source with
    | None -> Memo.return None
    | Some source ->
      let+ kind = Lock_dir.source_kind source >>| User_error.ok_exn in
      let root =
        match kind with
        | `Local (`Directory, path) -> Backing.Fs path
        | `Local (`File, _) | `Fetch ->
          Backing.Build (Fetch_rules.make_target ~kind:Dir source)
      in
      Some root
  ;;

  let lock_pkg t = t.lock_pkg
  let artifact_root t = t.artifact_root

  let depends t ~platform =
    Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform t.lock_pkg.depends ~platform
    |> Option.value ~default:[]
  ;;

  let dependencies t =
    let+ platform = Lock_dir.Sys_vars.solver_env in
    depends t ~platform
    |> Package.Name.Set.of_list_map ~f:(fun (dep : Dune_pkg.Lock_dir.Dependency.t) ->
      dep.name)
  ;;
end

let universe_candidates =
  let memo =
    Memo.create
      "package-candidates"
      ~input:(module Package_universe)
      (fun universe ->
         let+ lock_dir = Package_universe.lock_dir universe
         and+ lock_dir_path = Package_universe.lock_dir_path universe >>| Option.value_exn
         and+ platform = Lock_dir.Sys_vars.solver_env in
         Log.info
           "Package source candidates"
           [ "universe", Package_universe.to_dyn universe ];
         let root =
           match universe with
           | Dependencies context ->
             Path.Build.relative_fname
               (Context_name.build_dir context)
               Filename.pkg_dir_basename
           | Dev_tool tool -> Pkg_dev_tool.universe_install_path tool
         in
         Dune_pkg.Lock_dir.packages_on_platform lock_dir ~platform
         |> Package.Name.Map.map ~f:(fun (lock_pkg : Dune_pkg.Lock_dir.Pkg.t) ->
           let version =
             Option.some_if
               (Dune_pkg.Lock_dir.uses_versioned_paths lock_dir)
               lock_pkg.info.version
           in
           let files_dir =
             Dune_pkg.Lock_dir.Pkg.files_dir
               lock_pkg.info.name
               version
               ~lock_dir:lock_dir_path
             |> Path.as_in_build_dir_exn
           in
           let artifact_root =
             Path.Build.relative root (Package.Name.to_string lock_pkg.info.name)
           in
           { Candidate.lock_pkg; files_dir; artifact_root }))
  in
  Memo.exec memo
;;

let candidates context =
  Lock_dir.lock_dir_active context
  >>= function
  | false -> Memo.return Package.Name.Map.empty
  | true -> universe_candidates (Dependencies context)
;;

let dependencies =
  Per_context.create_by_name ~name:"mounted-package-dependencies" (fun context ->
    Memo.lazy_ ~name:"mounted-package-dependencies" (fun () ->
      let* candidates = candidates context in
      (* Polling the platform runs commands; do not without a lock dir. *)
      if Package.Name.Map.is_empty candidates
      then Memo.return Package.Name.Map.empty
      else
        let+ platform = Lock_dir.Sys_vars.solver_env in
        Package.Name.Map.map candidates ~f:(fun candidate ->
          Candidate.depends candidate ~platform
          |> List.filter_map ~f:(fun (dep : Dune_pkg.Lock_dir.Dependency.t) ->
            Option.some_if
              (not (Package.Name.equal dep.name Dune_pkg.Dune_dep.name))
              dep.name)))
    |> Memo.Lazy.force)
  |> Staged.unstage
;;

let find_candidate context name =
  let+ candidates = candidates context in
  Package.Name.Map.find candidates name
;;

let listing root =
  let rec loop local =
    let dir = Backing.append_local root local in
    let* contents = Backing.dir_contents dir in
    match contents with
    | Error error -> Memo.return (Error error)
    | Ok None ->
      (match dir with
       | Backing.Build dir ->
         Code_error.raise
           "Package source directory was not produced"
           [ "dir", Path.Build.to_dyn dir ]
       | Fs dir ->
         Memo.return
           (Error (Unix.ENOENT, "readdir", Path.Outside_build_dir.to_string dir)))
    | Ok (Some (files, dirs)) ->
      let files =
        Filename.Array.Set.to_list files
        |> List.map ~f:(fun file ->
          ( Backing.relative_fname dir file |> Backing.path
          , Path.Local.relative_fname local file ))
      in
      let+ subdirs =
        Filename.Array.Set.to_list dirs
        |> Memo.List.map ~f:(fun dir -> loop (Path.Local.relative_fname local dir))
      in
      let open Result.O in
      let+ subdirs = Result.List.all subdirs in
      { Source_input.files =
          files @ List.concat_map subdirs ~f:(fun { Source_input.files; _ } -> files)
      ; directories =
          local
          :: List.concat_map subdirs ~f:(fun { Source_input.directories; _ } ->
            directories)
      }
  in
  loop Path.Local.root
;;

let listing =
  let memo = Memo.create "package-source-listing" ~input:(module Backing) listing in
  fun ~loc root ->
    let+ contents = Memo.exec memo root in
    match contents with
    | Ok contents -> contents
    | Error error -> Backing.unreadable ~loc root error
;;

let source candidate =
  let* root = Candidate.source_root candidate in
  match root with
  | None -> Memo.return { Source_input.files = []; directories = [] }
  | Some root -> listing ~loc:(Candidate.source_loc candidate) root
;;

module Package_action = Dune_lang.Action

let default_package_variable ~present name =
  match Package_variable_name.to_string name with
  | "pinned" -> Some (OpamVariable.B false)
  | "enable" -> Some (OpamVariable.S (if present then "enable" else "disable"))
  | "installed" -> Some (OpamVariable.B present)
  | "build-id" ->
    (* Dune does not cache across Opam switches. This dummy value cannot
       collide with real Opam build IDs. *)
    Some (OpamVariable.S "d00ed00ed00ed00ed00ed00ed00ed00e")
  | _ -> None
;;

let global_variable ~solver_env (variable : Pform.Var.Pkg.t) =
  let system name =
    Dune_pkg.Solver_env.get solver_env name
    |> Option.map ~f:Dune_pkg.Variable_value.to_opam_variable_contents
    |> Option.value ~default:(OpamVariable.S "")
    |> Option.some
  in
  match variable with
  | Switch -> Some (OpamVariable.S "dune")
  | Jobs -> Some (OpamVariable.S (Int.to_string !Clflags.concurrency))
  | User -> Some (OpamVariable.S (Unix.getlogin ()))
  | Group -> Some (OpamVariable.S (Unix.getgid () |> Unix.getgrgid).gr_name)
  | Os Os -> system Package_variable_name.os
  | Os Os_version -> system Package_variable_name.os_version
  | Os Os_distribution -> system Package_variable_name.os_distribution
  | Os Os_family -> system Package_variable_name.os_family
  | Arch -> system Package_variable_name.arch
  | Sys_ocaml_version -> system Package_variable_name.sys_ocaml_version
  | Build | Prefix | Section_dir _ -> None
;;

type variable_resolution =
  | Resolved of OpamVariable.variable_contents option
  | Requires_package_build

let resolve_package_variable
      candidates
      candidate
      { Dune_pkg.Package_variable.scope; name }
  =
  let package =
    match scope with
    | Self -> Candidate.name candidate
    | Package package -> package
  in
  match Package.Name.Map.find candidates package with
  | None ->
    let+ packages = Dune_load.packages ()
    and+ direct = Candidate.dependencies candidate in
    if Package.Name.Set.mem direct package && Package.Name.Map.mem packages package
    then Requires_package_build
    else (
      let value =
        match Package_variable_name.to_string name with
        | "build-id" -> None
        | _ -> default_package_variable ~present:false name
      in
      Resolved value)
  | Some package ->
    let variables =
      Dune_pkg.Lock_dir.Pkg_info.variables (Candidate.lock_pkg package).info
    in
    Memo.return
      (match
         Option.first_some
           (Package_variable_name.Map.find variables name)
           (default_package_variable ~present:true name)
       with
       | Some value -> Resolved (Some value)
       | None -> Requires_package_build)
;;

let rec action_contains_source_transformation = function
  | Package_action.Patch _ | Substitute _ -> true
  | Progn actions | Concurrent actions | Pipe (_, actions) ->
    List.exists actions ~f:action_contains_source_transformation
  | With_accepted_exit_codes (_, action)
  | Chdir (_, action)
  | Setenv (_, _, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | No_infer action
  | Withenv (_, action)
  | When (_, action) -> action_contains_source_transformation action
  | _ -> false
;;

(* Each action is paired with the environment updates in its scope, innermost
   first, so that it can be run outside of its [progn]. *)
let rec flatten_progn envs = function
  | Package_action.Progn actions -> List.concat_map actions ~f:(flatten_progn envs)
  | Withenv (env, action) -> flatten_progn (env :: envs) action
  | action -> [ envs, action ]
;;

let in_scope (envs, action) =
  List.fold_left envs ~init:action ~f:(fun action env ->
    Package_action.Withenv (env, action))
;;

let expand_condition_pform candidates candidate solver_env unsupported ~source = function
  | Pform.Var (Pkg variable) ->
    let values =
      match global_variable ~solver_env variable with
      | Some value -> Variable.dune_value value
      | None ->
        unsupported := true;
        [ Value.String "" ]
    in
    Memo.return (Ok values)
  | Macro ({ macro = Pkg | Pkg_self; _ } as invocation) ->
    let loc = Dune_sexp.Template.Pform.loc source in
    let variable =
      match Dune_pkg.Package_variable.of_macro_invocation ~loc invocation with
      | Ok variable -> variable
      | Error `Unexpected_macro -> Code_error.raise "Unexpected package variable macro" []
    in
    let* resolution = resolve_package_variable candidates candidate variable in
    (match resolution with
     | Resolved (Some value) -> Variable.dune_value value |> Result.ok |> Memo.return
     | Resolved None -> Memo.return (Error (`Undefined_pkg_var variable.name))
     | Requires_package_build ->
       unsupported := true;
       Memo.return (Ok [ Value.String "" ]))
  | _ ->
    unsupported := true;
    Memo.return (Ok [ Value.String "" ])
;;

let eval_source_condition candidates candidate solver_env condition =
  let unsupported = ref false in
  let expand sw =
    String_expander.Memo.expand_result_deferred_concat
      sw
      ~mode:Many
      ~f:(expand_condition_pform candidates candidate solver_env unsupported)
  in
  let+ enabled =
    Slang_expand.eval_blang
      condition
      ~dir:(Path.build (Candidate.artifact_root candidate))
      ~f:expand
  in
  if !unsupported then None else Some enabled
;;

let local_path path =
  String_with_vars.text_only path
  |> Option.map ~f:(Path.Local.parse_string_exn ~loc:(String_with_vars.loc path))
;;

(* The patches a recipe applies before doing anything else. Substitutions read
   package variables while they run, so a recipe containing one is left to its
   opaque build. *)
let leading_patches candidates candidate solver_env = function
  | None | Some Dune_pkg.Lock_dir.Build_command.Dune -> Memo.return ([], [], true)
  | Some (Action action) ->
    let rec loop patches actions =
      let stop () =
        let actions = List.map actions ~f:in_scope in
        Memo.return
          ( List.rev patches
          , actions
          , not (List.exists actions ~f:action_contains_source_transformation) )
      in
      match actions with
      | (_, Package_action.Patch path) :: rest ->
        (match local_path path with
         | Some patch -> loop ((String_with_vars.loc path, patch) :: patches) rest
         | None -> stop ())
      | (envs, Package_action.When (condition, action)) :: rest
        when action_contains_source_transformation action ->
        let condition = Slang.Blang.remove_locs condition |> Slang.simplify_blang in
        eval_source_condition candidates candidate solver_env condition
        >>= (function
         | None -> stop ()
         | Some true -> loop patches (flatten_progn envs action @ rest)
         | Some false -> loop patches rest)
      | _ -> stop ()
    in
    loop [] (flatten_progn [] action)
;;

module Scan_dune_files = Source_tree.Dir.Make_map_reduce (Memo) (Monoid.Exists)

let prepared_source context name =
  Path.Build.L.relative
    Private_context.t.build_dir
    [ Context_name.to_string context
    ; ".dune-source"
    ; Package.Name.to_string name
    ; "source"
    ]
;;

(* How a package's source reaches its mount. *)
module Preparation = struct
  type t =
    | Opaque (** The package has no source to mount. *)
    | Plain of
        { dir : Backing.t
        ; native : bool
        } (** The external or fetched directory is used as it is. *)
    | Prepared of
        { patches : (Loc.t * Path.Local.t) list
        ; rest : Package_action.t list
        ; native : bool
        }
    (** [prepared_source] holds the fetched source with the package's lock
        directory files and extra sources added, then [patches] applied.
        [rest] is what remains of the build action after those patches.
        [native] is false when [rest] still transforms the source, so a dune
        project found in it cannot be loaded before the recipe has run. *)

  let patches = function
    | Opaque -> None
    | Plain _ -> Some []
    | Prepared { patches; rest = _; native = _ } -> Some patches
  ;;
end

let preparation_of_candidate candidates candidate =
  let* direct = Candidate.dependencies candidate in
  let candidates =
    Package.Name.Map.filteri candidates ~f:(fun name _ ->
      Package.Name.equal name (Candidate.name candidate)
      || Package.Name.Set.mem direct name)
  in
  let* platform = Lock_dir.Sys_vars.solver_env in
  let lock_pkg = Candidate.lock_pkg candidate in
  let build =
    Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform
      lock_pkg.build_command
      ~platform
  in
  let* patches, rest, native = leading_patches candidates candidate platform build in
  let* has_files =
    Build_system.directory_target_contents_opt ~dir:candidate.Candidate.files_dir
    >>| function
    | None -> false
    | Some (files, dirs) ->
      not (Filename.Array.Set.is_empty files && Filename.Array.Set.is_empty dirs)
  in
  if
    has_files
    || List.is_non_empty lock_pkg.info.extra_sources
    || List.is_non_empty patches
  then Memo.return (Preparation.Prepared { patches; rest; native })
  else
    let* root = Candidate.source_root candidate in
    match root with
    | None -> Memo.return Preparation.Opaque
    | Some dir ->
      let+ input = source candidate in
      if List.is_empty input.files then Preparation.Opaque else Plain { dir; native }
;;

let preparation =
  let memo =
    Memo.create
      "package-source-preparation"
      ~input:(module Context_and_package)
      (fun (context, name) ->
         let* candidates = candidates context in
         match Package.Name.Map.find candidates name with
         | None -> Memo.return None
         | Some candidate -> preparation_of_candidate candidates candidate >>| Option.some)
  in
  fun context name -> Memo.exec memo (context, name)
;;

let mount_backing context name =
  preparation context name
  >>| function
  | None | Some Opaque -> None
  | Some (Plain { dir; native = _ }) -> Some dir
  | Some (Prepared { patches = _; rest = _; native = _ }) ->
    Some (Backing.Build (prepared_source context name))
;;

let prepare context candidate =
  let name = Candidate.name candidate in
  Log.info
    "Prepare package sources"
    [ "context", Context_name.to_dyn context; "package", Package.Name.to_dyn name ];
  let* preparation = preparation context name >>| Option.value_exn in
  let* backing = mount_backing context name in
  match preparation, backing with
  | (Opaque | Plain { native = false; _ } | Prepared { native = false; _ }), _ | _, None
    -> Memo.return `Opam
  | (Plain { native = true; _ } | Prepared { native = true; _ }), Some backing_root ->
    let logical_root = Candidate.artifact_root candidate in
    let* tree =
      Source_tree.load_mounted
        ~loc:(Candidate.source_loc candidate)
        ~logical_root
        ~backing_root
    in
    let* has_dune_file =
      Scan_dune_files.map_reduce
        tree
        ~traverse:Source_dir_status.Set.all
        ~trace_event_name:"Package source classification"
        ~f:(fun dir ->
          match Source_tree.Dir.dune_file dir with
          | None -> Memo.return false
          | Some _ ->
            let project = Source_tree.Dir.project dir in
            (match
               Package.Name.Map.find (Dune_project.including_hidden_packages project) name
             with
             | None -> Memo.return false
             | Some package -> Dune_load.package_enabled package))
    in
    Memo.return (if has_dune_file then `Dune tree else `Opam)
;;

let find_locked =
  let memo =
    Memo.create
      "locked-package"
      ~input:(module Context_and_package)
      (fun (context, name) ->
         let* candidate = find_candidate context name in
         match candidate with
         | None -> Memo.return None
         | Some candidate ->
           let+ build = prepare context candidate in
           Some (candidate, build))
  in
  fun context name -> Memo.exec memo (context, name)
;;

module Paths = struct
  (* The [paths] of a package are the information about the artifacts
     that we know {e without} executing any commands. *)
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  let map_path t ~f =
    { t with
      source_dir = f t.source_dir
    ; target_dir = f t.target_dir
    ; install_roots = Lazy.map ~f:(Install.Roots.map ~f) t.install_roots
    ; install_paths = Lazy.map ~f:(Install.Paths.map ~f) t.install_paths
    ; prefix = f t.prefix
    }
  ;;

  let install_roots ~target_dir ~relative =
    Install.Roots.opam_from_prefix ~relative target_dir
  ;;

  let install_paths roots package ~relative = Install.Paths.make ~relative ~package ~roots

  let of_root name ~root ~source_dir ~install_dir ~relative =
    let source_dir = relative root source_dir in
    let target_dir = relative root install_dir in
    let install_roots = lazy (install_roots ~target_dir ~relative) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name ~relative) in
    { source_dir; target_dir; name; install_paths; install_roots; prefix = target_dir }
  ;;

  let with_prefix t prefix =
    let roots = lazy (install_roots ~target_dir:prefix ~relative:Path.relative) in
    { t with
      target_dir = prefix
    ; prefix
    ; install_roots = roots
    ; install_paths =
        lazy (install_paths (Lazy.force roots) t.name ~relative:Path.relative)
    }
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

let candidate_paths candidate =
  Paths.of_root
    (Candidate.name candidate)
    ~root:(Candidate.artifact_root candidate)
    ~source_dir:"."
    ~install_dir:"_install"
    ~relative:Path.Build.relative
;;

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

    let repr files_repr =
      Repr.record
        "install-cookie"
        [ Repr.field "files" files_repr ~get:(fun t -> t.files)
        ; Repr.field "variables" (Repr.list Variable.repr) ~get:(fun t -> t.variables)
        ]
    ;;
  end

  type t = Path.t list Section.Map.t Gen.t

  module Persistent = Persistent.Make (struct
      type nonrec t = (Section.t * Path.t list) list Gen.t

      let sharing = false
      let name = "INSTALL-COOKIE"
      let version = 4
      let repr = Gen.repr (Repr.list (Repr.pair Section.repr (Repr.list Path.repr)))
    end)

  let load_exn f =
    match Persistent.load f with
    | Some f -> { f with files = Section.Map.of_list_exn f.files }
    | None -> User_error.raise ~loc:(Loc.in_file f) [ Pp.text "unable to load" ]
  ;;

  let dump path (t : t) =
    Persistent.dump path { t with files = Section.Map.to_list t.files }
  ;;

  let read (paths : Path.t Paths.t) =
    let open Action_builder.O in
    let path = Paths.install_cookie paths in
    let+ () = path |> Dep.file |> Action_builder.dep in
    load_exn path
  ;;
end

module Dependency = struct
  type t =
    { paths : Path.t Paths.t
    ; files : Path.t list Section.Map.t
    ; variables : Variable.value Package_variable_name.Map.t
    ; exported_env : String_with_vars.t Dune_lang.Action.Env_update.t list
    ; export_paths : Path.t Paths.t
    }

  let load paths ~variables ~exported_env =
    let open Action_builder.O in
    let* { Install_cookie.Gen.files; variables = installed_variables } =
      Install_cookie.read paths
    in
    let+ () = Section.Map.values files |> List.concat |> Action_builder.paths in
    let variables =
      Package_variable_name.Map.superpose
        (Package_variable_name.Map.of_list_exn installed_variables)
        variables
    in
    { paths; files; variables; exported_env; export_paths = paths }
  ;;

  let native paths ~prefix ~variables ~exported_env =
    let paths = Paths.with_prefix paths prefix in
    { paths; files = Section.Map.empty; variables; exported_env; export_paths = paths }
  ;;

  let lib_root { paths; _ } = (Paths.install_roots paths).lib_root
  let info { paths; variables; _ } = paths.name, (variables, paths)

  let install_entries { paths; files; _ } =
    Section.Map.to_list files
    |> List.concat_map ~f:(fun (section, files) ->
      let prefix = Install.Paths.get (Paths.install_paths paths) section in
      List.map files ~f:(fun src ->
        let dst = Path.reach src ~from:prefix in
        Install.Entry.Expanded.make section ~dst ~kind:File src))
  ;;

  let binaries { paths; files; _ } =
    let { Install.Roots.bin; _ } = Paths.install_roots paths in
    Section.Map.Multi.find files Bin
    |> List.filter ~f:(fun path -> Path.equal (Path.parent_exn path) bin)
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
      | Jobs jobs -> Int.to_string jobs
      | Dir s | Path s -> Path.to_absolute_filename s)
    |> Bin.encode_strings
  ;;

  let of_env env : t =
    Env.to_map env |> Env.Map.map ~f:(fun value -> [ Value.String value ])
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
        let old_v =
          match old_v with
          | None | Some [] -> [ Value.String "" ]
          | Some v -> v
        in
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

module Layout = struct
  type t =
    { context : Context_name.t
    ; root : Path.Build.t
    ; dependencies : Dependency.t list
    }

  let dependencies { dependencies; _ } = dependencies

  let materialize context dependencies =
    let open Action_builder.O in
    let packages =
      Package.Name.Set.of_list_map dependencies ~f:(fun (dep : Dependency.t) ->
        dep.paths.name)
    in
    let+ files_by_package = Install_layout.deps context packages in
    let root = Install_layout.root context packages in
    let prefix = Path.build root in
    let dependencies =
      List.map dependencies ~f:(fun (dependency : Dependency.t) ->
        let files =
          Package.Name.Map.find files_by_package dependency.paths.name
          |> Option.value ~default:Section.Map.empty
        in
        { dependency with paths = Paths.with_prefix dependency.paths prefix; files })
    in
    { context; root; dependencies }
  ;;
end

module Pkg = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; universe : Package_universe.t
    ; depends : t list
    ; depexts : Depexts.t list
    ; lock_exported_env : String_with_vars.t Env_update.t list
    ; info : Pkg_info.t
    ; paths : Path.t Paths.t
    ; exported_env : string Env_update.t list
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

  let native_dependencies context packages =
    match packages with
    | [] -> Action_builder.return []
    | _ ->
      let names = Package.Name.Set.of_list_map packages ~f:(fun t -> t.info.name) in
      let prefix = Install_layout.root context names |> Path.build in
      let dependencies =
        List.map packages ~f:(fun t ->
          Dependency.native
            t.paths
            ~prefix
            ~variables:(Pkg_info.variables t.info)
            ~exported_env:t.lock_exported_env)
      in
      let open Action_builder.O in
      let+ layout = Layout.materialize context dependencies in
      Layout.dependencies layout
  ;;

  let kind t =
    match t.universe with
    | Dev_tool _ -> Memo.return `Opam
    | Dependencies context ->
      find_locked context t.info.name
      >>| (function
       | Some (_, `Dune _) -> `Dune
       | Some (_, `Opam) -> `Opam
       | None ->
         Code_error.raise
           "mounted package is not locked"
           [ "package", Package.Name.to_dyn t.info.name ])
  ;;

  let partition packages =
    Memo.parallel_map packages ~f:(fun t ->
      let+ kind = kind t in
      t, kind)
    >>| List.partition_map ~f:(function
      | t, `Dune -> Left t
      | t, `Opam -> Right t)
  ;;

  let dependency t =
    let open Action_builder.O in
    let* kind = Action_builder.of_memo (kind t) in
    match kind with
    | `Opam ->
      Dependency.load
        t.paths
        ~variables:(Pkg_info.variables t.info)
        ~exported_env:t.lock_exported_env
    | `Dune ->
      let+ dependencies =
        native_dependencies (Package_universe.context_name t.universe) [ t ]
      in
      List.hd dependencies
  ;;

  let dep t =
    let open Action_builder.O in
    let* kind = Action_builder.of_memo (kind t) in
    match kind with
    | `Opam -> Action_builder.path t.paths.target_dir
    | `Dune -> Action_builder.ignore (dependency t)
  ;;

  let deps context packages =
    let open Action_builder.O in
    let* native, opaque = Action_builder.of_memo (partition packages) in
    let* (_ : Dependency.t list) = native_dependencies context native in
    Action_builder.List.iter opaque ~f:dep
  ;;

  let dependencies context packages =
    let open Action_builder.O in
    let* native, _ = Action_builder.of_memo (partition packages) in
    let* native = native_dependencies context native in
    let native =
      Package.Name.Map.of_list_map_exn native ~f:(fun dependency ->
        dependency.paths.name, dependency)
    in
    Action_builder.List.map packages ~f:(fun t ->
      match Package.Name.Map.find native t.info.name with
      | Some dependency -> Action_builder.return dependency
      | None -> dependency t)
  ;;

  let package_deps t = deps (Package_universe.context_name t.universe) (deps_closure t)

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

module Action_expander = struct
  module Expander = struct
    include Expander0

    type t =
      { paths : Path.t Paths.t
      ; artifacts : Path.t Filename.Map.t Memo.t
      ; depends :
          (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
            Memo.t
      ; depexts : Depexts.t list
      ; context : Context_name.t
      ; version : Package_version.t
      ; env : Value.t list Env.Map.t
      }

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

    let expand_pkg (paths : Path.t Paths.t) (pform : Pform.Var.Pkg.t) =
      match pform with
      | Build -> Memo.return [ Value.Dir paths.source_dir ]
      | Prefix -> Memo.return [ Value.Dir paths.prefix ]
      | Jobs -> Memo.return [ Value.Jobs !Clflags.concurrency ]
      | Section_dir section ->
        let roots = Paths.install_roots paths in
        let dir = section_dir_of_root roots section in
        Memo.return [ Value.Dir dir ]
      | Switch | Os _ | Sys_ocaml_version | User | Arch | Group ->
        let+ solver_env = Lock_dir.Sys_vars.solver_env in
        global_variable ~solver_env pform |> Option.value_exn |> Variable.dune_value
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
        (match default_package_variable ~present variable_name with
         | Some v -> Memo.return @@ Ok (Variable.dune_value v)
         | None ->
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
          { env = _; paths; artifacts = _; context; depends; version = _; depexts = _ }
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
        | (String _ | Jobs _) as value ->
          let program = Value.to_string value ~dir:t.paths.source_dir in
          (match Filename.analyze_program_name program with
           | Relative_to_current_dir | Absolute ->
             let dir = t.paths.source_dir in
             Memo.return @@ Ok (Path.relative dir program)
           | In_path ->
             (match program with
              | "dune" ->
                let dune = Path.of_string Sys.executable_name in
                Memo.return @@ Ok dune
              | program ->
                let program = Filename.of_string_exn program in
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
                           ())))))
      in
      Result.map prog ~f:(map_exe t)
    ;;
  end

  module Substitute = struct
    include Substs.Make (Memo)

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
            (Variable.value Package_variable_name.Map.t * Path.t Paths.t)
              Package.Name.Map.t
        ; artifacts : Path.t Filename.Map.t
        ; src : 'src
        ; dst : 'dst
        }

      let name = "substitute"
      let version = 4
      let runs_process = false
      let can_run_in_action_runner = false
      let bimap t f g = { t with src = f t.src; dst = g t.dst }
      let is_useful_to_memoize = true

      let variable_value_repr =
        Repr.variant
          "pkg-variable-value"
          [ Repr.case "B" Bool.repr ~proj:(function
              | Variable.B x -> Some x
              | _ -> None)
          ; Repr.case "S" String.repr ~proj:(function
              | Variable.S x -> Some x
              | _ -> None)
          ; Repr.case
              "L"
              Repr.(list String.repr)
              ~proj:(function
                | Variable.L x -> Some x
                | _ -> None)
          ]
      ;;

      let variable_map_repr =
        Repr.view
          Repr.(list (pair Package_variable_name.repr variable_value_repr))
          ~to_:Package_variable_name.Map.to_list
      ;;

      let paths_repr = Repr.T3.repr Path.repr Path.repr Package.Name.repr

      let hash_input_repr =
        Repr.T4.repr
          paths_repr
          Repr.(list (pair Filename.repr Path.repr))
          Repr.(list (pair variable_map_repr paths_repr))
          Package_version.repr
      ;;

      let encode { expander; depends; artifacts; src; dst } input output : Sexp.t =
        let e =
          let paths (p : Path.t Paths.t) = p.source_dir, p.target_dir, p.name in
          Digest.repr
            hash_input_repr
            ( paths expander.paths
            , Filename.Map.to_list artifacts
            , Package.Name.Map.to_list_map depends ~f:(fun _ (m, p) -> m, paths p)
            , expander.version )
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
             Expander.expand_pform expander ~source pform
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
             | Slang.Undefined -> None
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
               | Path p | Dir p -> Path p
               | Jobs jobs -> Jobs jobs))
         in
         let ocamlfind_destdir = (Lazy.force expander.paths.install_roots).lib_root in
         Run_with_path.action
           ~depexts
           ~pkg:(expander.paths.name, prog_loc)
           exe
           args
           ~ocamlfind_destdir)
    | Progn t ->
      let+ args = Memo.parallel_map t ~f:(expand ~expander) in
      Action.Progn args
    | System arg ->
      let+ arg =
        Expander.expand_pform_gen ~mode:Single expander arg >>| Value.to_string ~dir
      in
      Action.System arg
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
      Action.Setenv (Env.Var.to_string k, v, action))
  ;;

  let for_recipe
        (paths : Path.t Paths.t)
        { Lock_dir.Pkg.info; depexts; _ }
        ~context
        ~env
        ~direct
        ~dependencies
    =
    let { Pkg_info.version; _ } = info in
    let variables = Pkg_info.variables info in
    let direct_dependencies =
      List.filter dependencies ~f:(fun (dep : Dependency.t) ->
        Package.Name.Set.mem direct dep.paths.name)
    in
    let depends =
      List.map direct_dependencies ~f:Dependency.info
      |> Package.Name.Map.of_list_exn
      |> fun deps -> Package.Name.Map.add_exn deps paths.name (variables, paths)
    in
    let artifacts =
      List.concat_map dependencies ~f:Dependency.binaries
      |> List.fold_left ~init:Filename.Map.empty ~f:(fun acc path ->
        Filename.Map.set
          acc
          (Filename.of_string_exn
             (Bin.strip_exe (Filename.to_string (Path.basename path))))
          path)
    in
    { Expander.paths
    ; depends = Memo.return depends
    ; artifacts = Memo.return artifacts
    ; depexts
    ; context
    ; version
    ; env = Value_list_env.of_env env
    }
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
      (match closure with
       | [] -> Memo.return []
       | (first : Pkg.t) :: _ ->
         Action_builder.evaluate_and_collect_facts
           (Pkg.dependencies (Package_universe.context_name first.universe) closure)
         >>| fst)
      |> Memo.map ~f:(fun dependencies ->
        List.fold_left
          dependencies
          ~init:empty
          ~f:(fun { binaries; dep_info } (dependency : Dependency.t) ->
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
              let name, info = Dependency.info dependency in
              Package.Name.Map.add_exn dep_info name info
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
    ; artifacts
    ; context
    ; depends
    ; depexts = pkg.depexts
    ; version = pkg.info.version
    ; env
    }
  ;;

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let+ value =
      let+ value = Expander.expand_pform_gen expander env.value ~mode:Single in
      value |> Value.to_string ~dir:expander.paths.source_dir
    in
    { env with value }
  ;;
end

let layout_env { Layout.context; root; dependencies } =
  let open Action_builder.O in
  let roots = Install.Roots.opam_from_prefix root ~relative:Path.Build.relative in
  let env = Install.Roots.add_to_env roots Env.empty |> Value_list_env.of_env in
  let depends =
    List.map dependencies ~f:Dependency.info |> Package.Name.Map.of_list_exn
  in
  Action_builder.List.fold_left dependencies ~init:env ~f:(fun env (dep : Dependency.t) ->
    let expander =
      let paths =
        Paths.map_path dep.export_paths ~f:(fun path ->
          Path.of_string (Path.to_absolute_filename path))
      in
      let depends = Package.Name.Map.set depends paths.name (dep.variables, paths) in
      { Action_expander.Expander.paths
      ; artifacts = Memo.return Filename.Map.empty
      ; depends = Memo.return depends
      ; depexts = []
      ; context
      ; version = Dune_pkg.Package_version.dev
      ; env
      }
    in
    let+ updates =
      Action_builder.of_memo
        (Memo.List.map dep.exported_env ~f:(Action_expander.exported_env expander))
    in
    List.fold_left updates ~init:env ~f:Env_update.set)
  >>| Value_list_env.to_env
;;

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

let is_compiler_version_relocatable (info : Pkg_info.t) =
  Pkg_toolchain.is_compiler_package_with_toolchains_enabled info.name
  &&
  match Package_version.compare info.version (Package_version.of_string "5.5.0") with
  | Lt -> false
  | Eq | Gt -> true
;;

let write_paths (universe : Package_universe.t) candidate =
  match universe with
  | Dependencies _ -> candidate_paths candidate
  | Dev_tool _ ->
    Paths.of_root
      (Candidate.name candidate)
      ~root:(Candidate.artifact_root candidate)
      ~source_dir:"source"
      ~install_dir:"target"
      ~relative:Path.Build.relative
;;

module rec Resolve : sig
  val resolve : Loc.t -> Package.Name.t -> Package_universe.t -> Pkg.t Memo.t
end = struct
  open Resolve

  module Input = struct
    type t = Package_universe.t * Package.Name.t

    let equal = Tuple.T2.equal Package_universe.equal Package.Name.equal
    let hash = Tuple.T2.hash Package_universe.hash Package.Name.hash
    let to_dyn = Tuple.T2.to_dyn Package_universe.to_dyn Package.Name.to_dyn
  end

  let has_relocatable_compiler_marker (info : Pkg_info.t) depends =
    is_relocatable_compiler_marker info.name
    || Pkg.top_closure depends
       |> List.exists ~f:(fun (pkg : Pkg.t) ->
         is_relocatable_compiler_marker pkg.info.name)
  ;;

  let resolve_impl (package_universe, package) =
    let* candidates = universe_candidates package_universe in
    match Package.Name.Map.find candidates package with
    | None -> Memo.return None
    | Some candidate ->
      let ({ Lock_dir.Pkg.info; exported_env; depexts; _ } as pkg) =
        Candidate.lock_pkg candidate
      in
      let* platform = Lock_dir.Sys_vars.solver_env in
      (* Lock packages may depend on workspace packages, which are not in the
         lock directory. *)
      let* depends =
        Candidate.depends candidate ~platform
        |> List.filter ~f:(fun (dep : Dune_pkg.Lock_dir.Dependency.t) ->
          Package.Name.Map.mem candidates dep.name)
        |> Memo.parallel_map ~f:(fun { Dune_pkg.Lock_dir.Dependency.name; loc } ->
          resolve loc name package_universe)
      in
      let id = Pkg.Id.gen () in
      let paths =
        let paths =
          Paths.map_path (write_paths package_universe candidate) ~f:Path.build
        in
        if
          (not (Pkg_toolchain.is_compiler_package_with_toolchains_enabled info.name))
          || is_compiler_version_relocatable info
          || has_relocatable_compiler_marker info depends
        then paths
        else (
          (* A compiler that cannot be moved is installed in the user's
             toolchain directory, so that is its prefix. *)
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
        ; universe = package_universe
        ; depends
        ; depexts
        ; lock_exported_env = exported_env
        ; paths
        ; info
        ; exported_env = []
        }
      in
      let+ exported_env =
        let expander =
          Action_expander.expander (Package_universe.context_name package_universe) t
        in
        Memo.parallel_map exported_env ~f:(Action_expander.exported_env expander)
      in
      Some { t with exported_env }
  ;;

  let resolve =
    let memo =
      Memo.create
        "pkg-resolve"
        ~input:(module Input)
        ~human_readable_description:(fun (_, package) ->
          Pp.textf "- package %s" (Package.Name.to_string package) |> Option.some)
        resolve_impl
    in
    fun loc package package_universe ->
      Memo.exec memo (package_universe, package)
      >>| function
      | Some s -> s
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf "Unknown package %S" (Package.Name.to_string package) ]
  ;;
end

module Install_action = struct
  (* The install action does the following:

     1. Runs the install action in the lock file (if it exists)
     2. Reads the .install file produced by the build command
     3. Discovers all the files produced by 1.
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
    let version = 2
    let runs_process = false
    let can_run_in_action_runner = false

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

    let is_useful_to_memoize = true

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
            |> Filename.to_string
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

    let collect_files ~root ~skip_dirs =
      List.iter skip_dirs ~f:(fun s -> assert (Path.equal root (Path.parent_exn s)));
      let path_of ~dir fname = Path.relative_fname (Path.relative root dir) fname in
      Fpath.traverse
        ~dir:(Path.to_string root)
        ~init:[]
        ~on_file:(fun ~dir fname acc -> path_of ~dir fname :: acc)
        ~enter_dir:(fun ~dir fname ->
          let path = path_of ~dir fname in
          not (List.mem skip_dirs path ~equal:Path.equal))
        ~on_error:
          (`Call
              (fun ~dir error acc ->
                if dir = "" then acc else Unix_error.Detailed.raise error))
        ()
    ;;

    let maybe_drop_sandbox_dir path =
      match Path.extract_build_context_dir_maybe_sandboxed path with
      | None -> path
      | Some (sandbox, source) ->
        let ctx =
          let name = Path.basename sandbox in
          Path.relative_fname (Path.build Path.Build.root) name
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
        let files =
          match section with
          | Lib_root ->
            collect_files ~root:path ~skip_dirs:[ get Toplevel; get Stublibs; get Lib ]
          | Share_root -> collect_files ~root:path ~skip_dirs:[ get Share ]
          | _ -> collect_files ~root:path ~skip_dirs:[]
        in
        files
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
              let perm =
                (Unix.stat (Path.to_string file)).st_perm |> Permissions.Mode.of_int
              in
              if Permissions.(test execute perm) then section' else section
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
          let perm = (Unix.stat dst).st_perm |> Permissions.Mode.of_int in
          Permissions.(add execute) perm |> Permissions.Mode.to_int
        in
        Unix.chmod dst permission
    ;;

    let read_variables config_file =
      match Fpath.exists (Path.to_string config_file) with
      | false -> []
      | true ->
        let config =
          let filename = Path.to_string config_file in
          match
            Io.read_file_exn config_file
            |> OpamFile.Dot_config.read_from_string
                 ~filename:(OpamFile.make (OpamFilename.of_string filename))
          with
          | s -> s
          | exception OpamPp.Bad_format (pos, message) ->
            let loc =
              Option.map
                pos
                ~f:(fun { OpamParserTypes.FullPos.filename = _; start; stop } ->
                  let file_contents = Io.read_file_exn config_file in
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
              let error =
                Pp.textf
                  "Error parsing %s"
                  (Path.basename config_file |> Filename.to_string)
              in
              match loc with
              | None -> error
              | Some loc ->
                (Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> User_message.Style.Loc))
                ++ error
            in
            User_error.raise [ message_with_loc; User_error.reason (Pp.verbatim message) ]
        in
        OpamFile.Dot_config.bindings config
        |> List.map ~f:(fun (name, value) -> Package_variable_name.of_opam name, value)
    ;;

    (* On Windows, .install files may omit .exe for bin/sbin entries.
       Try the .exe variant if the original doesn't exist, mirroring opam. *)
    let maybe_add_exe src (entry : Path.t Install.Entry.Expanded.t) =
      match
        match Sys.win32, entry.section with
        | true, (Bin | Sbin) when not (Fpath.exists (Path.to_string src)) ->
          let src_exe_str = Bin.add_exe (Path.to_string src) in
          if Fpath.exists src_exe_str then Some src_exe_str else None
        | _ -> None
      with
      | None -> src, entry
      | Some src_exe_str ->
        ( Path.of_string src_exe_str
        , Install.Entry.map_dst entry ~f:Install.Entry.Dst.maybe_add_exe )
    ;;

    let install_entry
          ~src
          ~install_file
          ~target_dir
          (entry : Path.t Install.Entry.Expanded.t)
      =
      let src, entry = maybe_add_exe src entry in
      match Fpath.exists (Path.to_string src), entry.optional with
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
         Io.portable_hardlink ~overwrite:true ~src ~dst);
        maybe_set_executable entry.section dst;
        Some (entry.section, dst)
    ;;

    let resolve_symlinks_in root =
      let on_symlink ~dir fname () =
        let path = Filename.concat root (Filename.append dir fname) in
        match Fpath.follow_symlink path with
        | Error (Unix_error e) -> Unix_error.Detailed.raise e
        | Error Not_a_symlink ->
          Code_error.raise
            "resolve_symlinks_in: not a symlink"
            [ "path", Dyn.string path ]
        | Error Max_depth_exceeded ->
          User_error.raise
            [ Pp.textf
                "Unable to resolve symlink %s: too many levels of symbolic links"
                path
            ]
        | Ok resolved ->
          (match Unix.lstat resolved with
           | { Unix.st_kind = S_REG; _ } ->
             (* CR-someday rgrinberg: pass chmod:true here? *)
             Fpath.unlink_exn path;
             Io.portable_hardlink
               ~overwrite:true
               ~src:(Path.of_string resolved)
               ~dst:(Path.of_string path)
           | _ -> ());
          (), None
      in
      Fpath.traverse
        ~dir:root
        ~init:()
        ~on_other:`Ignore
        ~on_symlink:(`Call on_symlink)
        ()
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
                ~source_dir:"source"
                ~install_dir:(Filename.to_string (Path.basename target_dir))
                ~relative:Path.relative
              |> Paths.install_paths
            in
            section_map_of_dir install_paths
        in
        let+ from_install_file =
          (* Read all the artifacts from the .install file produced by
             the build command. This is the happy path where we don't guess
             anything. *)
          Async.async (fun () -> Fpath.exists (Path.to_string install_file))
          >>= function
          | false -> Fiber.return Section.Map.empty
          | true ->
            let* map =
              let install_entries =
                let dir = Path.parent_exn install_file in
                Install.Entry.Expanded.load_install_file install_file (fun local ->
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
              Async.async (fun () ->
                (* CR-someday rgrinberg: pass chmod:true here? *)
                Fpath.unlink_exn (Path.to_string install_file))
            in
            map
        in
        let declared =
          Section.Map.values from_install_file |> List.concat |> Path.Set.of_list
        in
        let from_install_action =
          Section.Map.map from_install_action ~f:(fun files ->
            List.filter files ~f:(fun file -> not (Path.Set.mem declared file)))
        in
        Section.Map.union from_install_action from_install_file ~f:(fun _ x y ->
          Some (x @ y))
        |> Section.Map.map ~f:(List.sort_uniq ~compare:Path.compare)
      in
      let* cookies =
        let+ variables = Async.async (fun () -> read_variables config_file) in
        { Install_cookie.Gen.files; variables }
      in
      Async.async (fun () ->
        (* Resolve symlinks in target_dir so that the cache can store them. The
         dune cache doesn't support symlinks, so we replace them with hardlinks
         to their targets. *)
        if Fpath.exists (Path.Build.to_string target_dir)
        then resolve_symlinks_in (Path.Build.to_string target_dir);
        (* Produce the cookie file in the standard path *)
        let cookie_file = Path.build @@ Paths.install_cookie' target_dir in
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

module Copy_source = struct
  module Spec = struct
    type ('path, 'target) t = 'path * 'target

    let name = "copy-package-source"
    let version = 1
    let is_useful_to_memoize = true
    let runs_process = false
    let can_run_in_action_runner = false
    let bimap (src, dst) f g = f src, g dst
    let encode (src, dst) input output = Sexp.List [ input src; output dst ]

    let action (src, dst) ~ectx:_ ~eenv:_ =
      Async.async (fun () ->
        Io.copy_file
          ~overwrite:true
          ~chmod:(Permissions.add Permissions.write)
          ~src
          ~dst:(Path.build dst)
          ())
    ;;
  end

  include Action_ext.Make (Spec)
end

let execute ~expander ~write_paths ~source ~build ~install =
  let copies = Action_builder.map source ~f:Action.Full.make in
  let install_dirs =
    match install with
    | [] -> []
    | _ ->
      let paths = Paths.install_paths write_paths in
      [ Install_action.installable_sections
        |> List.map ~f:(fun section -> Action.mkdir (Install.Paths.get paths section))
        |> Action.progn
        |> Action.Full.make
        |> Action_builder.With_targets.return
      ]
  in
  let finish =
    Install_action.action
      write_paths
      (match install with
       | [] -> `No_install_action
       | _ -> `Has_install_action)
      ~prefix_outside_build_dir:
        (Path.as_outside_build_dir expander.Action_expander.Expander.paths.prefix)
    |> Action.Full.make
    |> Action_builder.With_targets.return
  in
  Action_builder.progn
    ((Action_builder.with_no_targets copies :: build)
     @ install_dirs
     @ install
     @ [ finish ])
  |> Action_builder.With_targets.map ~f:(fun action ->
    Action.Full.add_env (Value_list_env.to_env expander.env) action
    |> Action.Full.add_sandbox Sandbox_config.needs_sandboxing
    |> Action.Full.disable_sandbox_policy)
;;

let execute_recipe ~build ~install ~expander ~write_paths ~source =
  let { Action_expander.Expander.paths; _ } = expander in
  let expand action =
    let+ action = Action_expander.expand action ~expander in
    Action.chdir paths.source_dir action
    |> Action.Full.make
    |> Action_builder.With_targets.return
  in
  let* build =
    match build with
    | None -> Memo.return []
    | Some (Build_command.Action action) -> expand action >>| List.singleton
    | Some Dune ->
      Command.run_dyn_prog
        (Action_builder.return (Ok (Path.of_string Sys.executable_name)))
        ~dir:paths.source_dir
        [ A "build"; A "-p"; A (Package.Name.to_string paths.name) ]
      |> List.singleton
      |> Memo.return
  and* install =
    match install with
    | None -> Memo.return []
    | Some action -> expand action >>| List.singleton
  in
  execute ~expander ~write_paths ~source ~build ~install |> Memo.return
;;

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
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

module Prepare_source = struct
  module A = Action_ext.Make (struct
      type ('path, 'target) t = 'path

      let name = "prepare-package-source"
      let version = 1
      let runs_process = false
      let can_run_in_action_runner = false
      let is_useful_to_memoize = false
      let bimap dir f _ = f dir
      let encode dir path _ = path dir

      let action source_dir ~ectx:_ ~eenv:_ =
        Fiber.Temp.with_temp_dir
          ~parent_dir:(Path.parent_exn source_dir)
          ~prefix:".dune"
          ~suffix:"copy"
          ~f:(fun tmp ->
            let tmp = Result.ok_exn tmp in
            Async.async (fun () ->
              let tmp = Path.relative tmp "source" in
              let make_writable = Permissions.add Permissions.write in
              let copy ~dir file () =
                let path = Path.relative_fname (Path.relative source_dir dir) file in
                Io.copy_file ~overwrite:false ~chmod:make_writable ~src:path ~dst:tmp ();
                Fpath.rename_exn (Path.to_string tmp) (Path.to_string path)
              in
              Fpath.traverse
                ~dir:(Path.to_string source_dir)
                ~init:()
                ~on_file:copy
                ~on_symlink:
                  (`Call
                      (fun ~dir file () ->
                        copy ~dir file ();
                        (), None))
                ()))
      ;;
    end)

  let action dir =
    A.action dir
    |> Action.Full.make ~sandbox:Sandbox_config.needs_sandboxing
    |> Action_builder.With_targets.return
  ;;
end

let dune_dep =
  lazy (Sys.executable_name |> Path.External.of_string |> Path.external_ |> Dep.file)
;;

(* The fetched source of a package with its lock directory files and extra
   sources laid over it. *)
let candidate_source_input candidate =
  let open Action_builder.O in
  let* source = Action_builder.of_memo (source candidate) in
  let { Candidate.files_dir; _ } = candidate in
  let+ files =
    Action_builder.of_memo
      (Memo.of_thunk (fun () ->
         let deps, files = files files_dir in
         Memo.return (files, deps)))
    |> Action_builder.dyn_deps
  in
  let files =
    source.Source_input.files
    @ Path.Set.to_list_map files ~f:(fun src ->
      src, Path.drop_prefix_exn src ~prefix:(Path.build files_dir))
    @ List.map
        (Candidate.lock_pkg candidate).info.extra_sources
        ~f:(fun (local, source) ->
          let path =
            match Dune_pkg.Source.kind source with
            | `Directory_or_archive path -> Path.external_ path
            | `Fetch -> Fetch_rules.make_target ~kind:File source |> Path.build
          in
          path, local)
  in
  { source with files }
;;

let prepared_source_rule candidate ~target ~patches =
  let build =
    let open Action_builder.O in
    let* { Source_input.files; directories } = candidate_source_input candidate in
    let+ () = Action_builder.paths (List.map files ~f:fst) in
    let copies =
      Action.progn
        (List.map (Path.Local.root :: directories) ~f:(fun local ->
           Action.mkdir (Path.Build.append_local target local))
         @ List.map files ~f:(fun (src, local) ->
           let dst = Path.Build.append_local target local in
           Action.progn
             [ Action.mkdir (Path.Build.parent_exn dst)
             ; Action.remove_tree dst
             ; Copy_source.action (src, dst)
             ]))
    in
    let patches =
      List.map patches ~f:(fun (_loc, patch) ->
        Dune_patch.action ~patch:(Path.build (Path.Build.append_local target patch))
        |> Action.chdir (Path.build target))
    in
    Action.progn (copies :: patches)
    |> Action.Full.make ~sandbox:Sandbox_config.needs_sandboxing
  in
  Action_builder.with_no_targets build
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
  |> rule
       ~loc:
         (match patches with
          | (loc, _) :: _ -> Dune_pkg.Lock_dir.loc_in_source_tree loc
          | [] -> Loc.none)
;;

let gen_prepared_source_rules context ~dir components =
  let module Gen_rules = Build_config.Gen_rules in
  match components with
  | [] -> Gen_rules.make_empty ~dir Subdir_set.all |> Memo.return
  | [ name ] ->
    let name = Package.Name.of_string name in
    let+ candidate = find_candidate context name
    and+ preparation = preparation context name in
    (match candidate, Option.bind preparation ~f:Preparation.patches with
     | Some candidate, Some patches ->
       let target = prepared_source context name in
       Gen_rules.make
         ~directory_targets:(Path.Build.Map.singleton target Loc.none)
         (Rules.collect_unit (fun () -> prepared_source_rule candidate ~target ~patches))
     | _ -> Gen_rules.no_rules)
  | _ -> Gen_rules.Redirect_to_parent Gen_rules.Rules.empty |> Memo.return
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

let all_deps universe =
  let* candidates = universe_candidates universe in
  Package.Name.Map.keys candidates
  |> Memo.parallel_map ~f:(fun package -> Resolve.resolve Loc.none package universe)
  >>| Pkg.top_closure
;;

let setup_pkg_install_alias =
  let build_packages_of_context ctx_name =
    (* Fetching the package target implies that we will also fetch the extra
       sources. *)
    let open Action_builder.O in
    let* packages = Action_builder.of_memo (all_deps (Dependencies ctx_name)) in
    Pkg.deps ctx_name packages
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

let resolve_pkg_dep context (loc, package_name) =
  Resolve.resolve loc package_name (Package_universe.Dependencies context)
;;

let dependency context candidate =
  Action_builder.of_memo (resolve_pkg_dep context (Loc.none, Candidate.name candidate))
  |> Action_builder.bind ~f:Pkg.dependency
;;

let find_opaque context name =
  let* locked = find_locked context name in
  match locked with
  | None | Some (_, `Dune _) -> Memo.return None
  | Some (candidate, `Opam) ->
    let root = (candidate_paths candidate).target_dir in
    Memo.return (Some (root, dependency context candidate))
;;

(* The rule building a package by its recipe. [source] depends on the package's
   source files. [inputs] are the package, the names of the dependencies whose
   variables the recipe can read, every dependency, and the environment they
   make. *)
let recipe_rule universe candidate ~build ~source ~inputs =
  let ({ Lock_dir.Pkg.info; install_command; _ } as lock_pkg) =
    Candidate.lock_pkg candidate
  in
  let write_paths = write_paths universe candidate in
  let* depends = Candidate.dependencies candidate
  and* platform = Lock_dir.Sys_vars.solver_env in
  let install =
    Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform install_command ~platform
    |> Option.map ~f:relocate
  in
  let expander =
    let open Action_builder.O in
    let+ { Pkg.paths; _ }, direct, dependencies, env = inputs in
    Action_expander.for_recipe
      paths
      lock_pkg
      ~context:(Package_universe.context_name universe)
      ~env
      ~direct
      ~dependencies
  in
  let progress =
    Pkg_build_progress.progress_action info.name info.version `Building
    |> Action.Full.make
    |> Action_builder.With_targets.return
  in
  let build =
    let open Action_builder.O in
    let* expander = expander in
    let* action =
      Action_builder.of_memo
        (execute_recipe ~build ~install ~expander ~write_paths ~source)
    in
    let* () =
      if Package.Name.Set.mem depends Dune_pkg.Dune_dep.name
      then Action_builder.deps (Dep.Set.singleton (Lazy.force dune_dep))
      else Action_builder.return ()
    in
    let prepare_source =
      match build, install with
      | Some (Action _), _ | _, Some _ ->
        [ Prepare_source.action (Path.build write_paths.source_dir) ]
      | (None | Some Dune), None -> []
    in
    let { Action_builder.With_targets.build; targets = _ } =
      Action_builder.progn (prepare_source @ [ progress; action ])
    in
    build
  in
  Action_builder.with_no_targets build
  |> Action_builder.With_targets.add_directories
       ~directory_targets:[ write_paths.target_dir ]
  |> rule ~loc:Loc.none
;;

(* What remains of a package's build command once its source is prepared: a
   prepared source already has the recipe's leading patches applied. *)
let remaining_build candidate (preparation : Preparation.t option) ~platform =
  match
    ( preparation
    , Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform
        (Candidate.lock_pkg candidate).build_command
        ~platform
      |> Option.map ~f:relocate_build )
  with
  | Some (Prepared { rest; patches = _; native = _ }), Some (Action _) ->
    Some (Build_command.Action (Progn rest))
  | _, build -> build
;;

let gen_mounted_rule context candidate ~dependencies =
  let name = Candidate.name candidate in
  let* platform = Lock_dir.Sys_vars.solver_env
  and* preparation = preparation context name
  and* backing = mount_backing context name in
  let build = remaining_build candidate preparation ~platform in
  let source =
    let open Action_builder.O in
    match backing with
    | None -> Action_builder.return (Action.mkdir (candidate_paths candidate).source_dir)
    | Some backing ->
      let* { Source_input.files; directories = _ } =
        Action_builder.of_memo (listing ~loc:(Candidate.source_loc candidate) backing)
      in
      let+ () =
        List.map files ~f:(fun (_, local) ->
          Path.build
            (Path.Build.append_local (candidate_paths candidate).source_dir local))
        |> Action_builder.paths
      in
      Action.progn []
  in
  let inputs =
    let open Action_builder.O in
    let* direct, layout = dependencies in
    let* pkg = Action_builder.of_memo (resolve_pkg_dep context (Loc.none, name)) in
    let+ env =
      match Layout.dependencies layout with
      | [] -> Action_builder.return (Global.env ())
      | _ ->
        (* As [Pkg.exported_value_env]: the packages replace the variables of
           the ambient environment, except PATH which they extend. *)
        let+ env = layout_env layout in
        Env.extend_env (Global.env ()) (Env.remove env ~var:Env_path.var)
        |> Env_path.cons_multi ~dirs:(Env_path.path env)
    in
    ( pkg
    , direct
    , Layout.dependencies layout
    , Env.extend_env env (Value_list_env.to_env (Pkg.base_env pkg)) )
  in
  recipe_rule (Dependencies context) candidate ~build ~source ~inputs
;;

(* A package of a dev tool is built by its recipe, from a private copy of its
   source. *)
let gen_dev_tool_rule tool name =
  let universe = Package_universe.Dev_tool tool in
  let* candidates = universe_candidates universe in
  match Package.Name.Map.find candidates name with
  | None -> Memo.return ()
  | Some candidate ->
    let* platform = Lock_dir.Sys_vars.solver_env in
    let { Paths.source_dir; _ } = write_paths universe candidate in
    let* pkg = Resolve.resolve Loc.none (Candidate.name candidate) universe
    and* preparation = preparation_of_candidate candidates candidate in
    let build = remaining_build candidate (Some preparation) ~platform in
    let* source =
      match Preparation.patches preparation with
      | None -> Memo.return (Action_builder.return (Action.mkdir source_dir))
      | Some patches ->
        let+ () = prepared_source_rule candidate ~target:source_dir ~patches in
        let open Action_builder.O in
        let+ () = Action_builder.path (Path.build source_dir) in
        Action.progn []
    in
    let inputs =
      let open Action_builder.O in
      let closure = Pkg.deps_closure pkg in
      (* The whole installation of each dependency, not only the files its
         cookie lists. *)
      let* () = Pkg.package_deps pkg in
      let+ dependencies =
        Pkg.dependencies (Package_universe.context_name universe) closure
      in
      ( pkg
      , Package.Name.Set.of_list_map closure ~f:(fun (pkg : Pkg.t) -> pkg.info.name)
      , dependencies
      , Pkg.exported_env pkg )
    in
    recipe_rule universe candidate ~build ~source ~inputs
;;

let setup_rules ~components ~dir ctx =
  (* Note that the path components in the following patterns must
     correspond to the paths returned by [write_paths]. The string
     ".dev-tool" is hardcoded into several patterns, and must match
     the value of [Pkg_dev_tool.install_path_base_dir_name]. *)
  assert (String.equal Pkg_dev_tool.install_path_base_dir_name ".dev-tool");
  match Context_name.is_default ctx, components with
  | true, [ ".dev-tool"; dev_tool_package_name; package ] ->
    let+ rules =
      Rules.collect_unit (fun () ->
        gen_dev_tool_rule
          (Package.Name.of_string dev_tool_package_name |> Pkg_dev_tool.of_package_name)
          (Package.Name.of_string package))
    in
    Gen_rules.make ~directory_targets:(Rules.directory_targets rules) (Memo.return rules)
  | true, ([ ".dev-tool" ] | [ ".dev-tool"; _ ]) ->
    Gen_rules.make_empty ~dir Subdir_set.all |> Memo.return
  | _, ".pkg" :: _ :: _ ->
    Memo.return (Gen_rules.Redirect_to_parent Gen_rules.Rules.empty)
  | true, ".dev-tool" :: _ :: _ :: _ :: _ ->
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
            let+ cookie = Install_cookie.read pkg.paths in
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

(* Every caller must get the same memoized builder: building the toolchain runs
   the compiler to read its configuration. *)
let ocaml_toolchain =
  let memo =
    Memo.create "pkg-ocaml-toolchain" ~input:(module Context_name) ocaml_toolchain
  in
  Memo.exec memo
;;

let compiler_packages =
  Memo.create
    "compiler-packages"
    ~input:(module Context_name)
    (fun context ->
       Lock_dir.lock_dir_active context
       >>= function
       | false -> Memo.return []
       | true ->
         let* lock_dir = Lock_dir.get_exn context in
         (match lock_dir.ocaml with
          | None -> Memo.return []
          | Some ocaml ->
            let+ pkg = resolve_pkg_dep context ocaml in
            pkg :: Pkg.deps_closure pkg))
;;

let compiler_dep context =
  Action_builder.bind
    (Action_builder.of_memo (Memo.exec compiler_packages context))
    ~f:(Pkg.deps context)
;;

let compiler_tool_dep context prog =
  match Option.bind (Path.as_in_build_dir prog) ~f:Path.Build.split_first_component with
  | None -> Action_builder.return ()
  | Some (prog_context, dir) ->
    (match package_of_dir_in_context dir with
     | Some package
       when String.equal
              (Filename.to_string prog_context)
              (Context_name.to_string context) ->
       Action_builder.bind
         (Action_builder.of_memo (Memo.exec compiler_packages context))
         ~f:(fun compiler ->
           if
             List.exists compiler ~f:(fun (pkg : Pkg.t) ->
               Package.Name.equal pkg.info.name package)
           then compiler_dep context
           else Action_builder.return ())
     | Some _ | None -> Action_builder.return ())
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

(* [packages] with the package installing the compiler and its dependencies.
   Their binaries are visible from every directory, whatever it declares. *)
let declared_packages context (packages : Package.Name.Selection.t) =
  match packages with
  | All -> dependencies context >>| Package.Name.Set.of_keys
  | Only packages ->
    let+ compiler = Memo.exec compiler_packages context in
    Package.Name.Set.union
      packages
      (Package.Name.Set.of_list_map compiler ~f:(fun (pkg : Pkg.t) -> pkg.info.name))
;;

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
       (* A lookup never builds an opaque package to see what it installs:
          only those the directory declares, and the compiler. *)
       let* declared = declared_packages context packages in
       let* packages = project_deps ~packages:(Only declared) context in
       let* _, opaque = Pkg.partition packages in
       let+ { binaries; dep_info = _ } =
         Action_expander.Artifacts_and_deps.of_closure opaque
       in
       binaries)
;;

let which ~packages context program =
  let+ binaries = Memo.exec lock_dir_binaries (context, packages) in
  Filename.Map.find binaries program
;;

(* For each locked package whose dependency closure reaches the workspace, the
   workspace packages it reaches. Building such a package while generating the
   rules of one of those packages would re-enter them. *)
let workspace_dependencies =
  let memo =
    Memo.create
      "locked-packages-workspace-dependencies"
      ~input:(module Context_name)
      (fun context ->
         let+ depends = dependencies context
         and+ workspace = Dune_load.packages () in
         let step reached =
           Package.Name.Map.filter_map depends ~f:(fun depends ->
             let workspace_deps =
               List.fold_left depends ~init:Package.Name.Set.empty ~f:(fun acc dep ->
                 if Package.Name.Map.mem workspace dep
                 then Package.Name.Set.add acc dep
                 else (
                   match Package.Name.Map.find reached dep with
                   | None -> acc
                   | Some deps -> Package.Name.Set.union acc deps))
             in
             Option.some_if
               (not (Package.Name.Set.is_empty workspace_deps))
               workspace_deps)
         in
         let rec loop reached =
           let next = step reached in
           if Package.Name.Map.equal ~equal:Package.Name.Set.equal next reached
           then reached
           else loop next
         in
         loop Package.Name.Map.empty)
  in
  Memo.exec memo
;;

let ocamlpath universe =
  let+ all_project_deps = all_deps universe in
  let env = Pkg.build_env_of_deps all_project_deps in
  Env.Map.find env Dune_findlib.Config.ocamlpath_var
  |> Option.value ~default:[]
  |> List.map ~f:(function
    | Value.Dir p | Path p -> p
    | String s -> Path.of_filename_relative_to_initial_cwd s
    | Jobs jobs -> Path.of_filename_relative_to_initial_cwd (Int.to_string jobs))
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
  let+ pkg = Resolve.resolve Loc.none package_name (Dev_tool tool) in
  Pkg.exported_env pkg
;;

let exported_env context =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf "lock directory environment for context %S" (Context_name.to_string context))
  @@ fun () ->
  let+ all_project_deps = all_project_deps context in
  Pkg.build_env_of_deps all_project_deps |> Value_list_env.to_env
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
         Pkg.build_env_of_deps deps |> Value_list_env.to_env)
;;

let env_for_packages ~(packages : Package.Name.Selection.t) context =
  Memo.exec lock_dir_package_env (context, packages)
;;

let bin_paths_for_packages ~packages context =
  let+ env = env_for_packages ~packages context in
  Env_path.path env
;;

let compiler_bin_paths context =
  lock_dir_active context
  >>= function
  | false -> Memo.return []
  | true ->
    let* lock_dir = Lock_dir.get_exn context in
    (match lock_dir.ocaml with
     | None -> Memo.return []
     | Some (_, ocaml) ->
       bin_paths_for_packages ~packages:(Only (Package.Name.Set.singleton ocaml)) context)
;;

let find_package ctx pkg =
  lock_dir_active ctx
  >>= function
  | false -> Memo.return None
  | true ->
    let+ pkg = resolve_pkg_dep ctx (Loc.none, pkg) in
    let dependencies =
      Pkg.deps_closure pkg
      |> Package.Name.Set.of_list_map ~f:(fun (pkg : Pkg.t) -> pkg.info.name)
    in
    Some
      ( dependencies
      , let open Action_builder.O in
        let* () = Pkg.package_deps pkg in
        Pkg.dep pkg )
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
  let* { Dependency.paths; files; _ } = Pkg.dependency pkg in
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
