open Stdune
module Package_name = Dune_lang.Package_name

module Global : sig
  val with_switch_state :
       switch_name:string
    -> f:(OpamStateTypes.unlocked OpamStateTypes.switch_state -> 'a)
    -> 'a
end = struct
  let initialized = ref false

  let ensure_initialized () =
    if not !initialized then (
      OpamSystem.init ();
      let root = OpamStateConfig.opamroot () in
      let (_ : OpamFile.Config.t option) =
        OpamStateConfig.load_defaults ~lock_kind:`Lock_read root
      in
      OpamFormatConfig.init ();
      OpamCoreConfig.init ~safe_mode:true ();
      OpamRepositoryConfig.init ();
      OpamStateConfig.init ~root_dir:root ();
      initialized := true)

  let with_switch_state ~switch_name ~f =
    ensure_initialized ();
    try
      OpamGlobalState.with_ `Lock_read (fun global_state ->
          Dune_console.print
            [ Pp.textf "Using opam installation: %s"
                (OpamFilename.Dir.to_string global_state.root)
            ];
          let switch = OpamSwitch.of_string switch_name in
          if not (OpamGlobalState.switch_exists global_state switch) then
            User_error.raise
              [ Pp.textf
                  "There is no opam switch named %s. Run `opam switch list` to \
                   see a list of switches."
                  (String.maybe_quoted switch_name)
              ];
          OpamSwitchState.with_ ~switch `Lock_read global_state
            (fun switch_state ->
              Dune_console.print
                [ Pp.textf "Using opam switch: %s"
                    (OpamSwitch.to_string switch_state.switch)
                ];
              f switch_state))
    with
    | OpamPp.Bad_version _ as bad_version ->
      User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_version) ]
    | OpamGlobalState.Configuration_error configuration_error ->
      User_error.raise
        [ Pp.text
            (OpamGlobalState.string_of_configuration_error configuration_error)
        ]
end

module Local_repo = struct
  let ( / ) = Filename.concat

  type t = { packages_dir_path : Filename.t }

  let minimum_opam_version = OpamVersion.of_string "2.0"

  let validate_repo_file opam_repo_dir_path =
    let opam_repo_file_path = opam_repo_dir_path / "repo" in
    let repo =
      try
        OpamFilename.raw opam_repo_file_path
        |> OpamFile.make |> OpamFile.Repo.read
      with
      | OpamSystem.Internal_error message ->
        User_error.raise [ Pp.text message ]
      | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as
        bad_format_exn ->
        User_error.raise
          [ Pp.text (OpamPp.string_of_bad_format bad_format_exn) ]
      | unexpected_exn ->
        Code_error.raise
          (Printf.sprintf
             "Unexpected exception raised while validating opam repo file %s"
             (String.maybe_quoted opam_repo_file_path))
          [ ("exception", Exn.to_dyn unexpected_exn) ]
    in
    match OpamFile.Repo.opam_version repo with
    | None ->
      User_error.raise
        [ Pp.textf "The file %s lacks an \"opam-version\" field."
            (String.maybe_quoted opam_repo_file_path)
        ]
        ~hints:
          [ Pp.textf "Add `opam-version: \"%s\"` to the file."
              (OpamVersion.to_string minimum_opam_version)
          ]
    | Some opam_version ->
      if OpamVersion.compare opam_version minimum_opam_version < 0 then
        User_error.raise
          [ Pp.textf
              "The file %s specifies an opam-version which is too low (%s). \
               The minimum opam-version is %s."
              (String.maybe_quoted opam_repo_file_path)
              (OpamVersion.to_string opam_version)
              (OpamVersion.to_string minimum_opam_version)
          ]
          ~hints:
            [ Pp.textf
                "Change the opam-version field to `opam-version: \"%s\"`."
                (OpamVersion.to_string minimum_opam_version)
            ]

  let of_opam_repo_dir_path opam_repo_dir_path =
    if not (Sys.file_exists opam_repo_dir_path) then
      User_error.raise
        [ Pp.textf "%s does not exist" (String.maybe_quoted opam_repo_dir_path)
        ];
    if not (Sys.is_directory opam_repo_dir_path) then
      User_error.raise
        [ Pp.textf "%s is not a directory"
            (String.maybe_quoted opam_repo_dir_path)
        ];
    let packages_dir_path = opam_repo_dir_path / "packages" in
    if
      not
        (Sys.file_exists packages_dir_path && Sys.is_directory packages_dir_path)
    then
      User_error.raise
        [ Pp.textf
            "%s doesn't look like a path to an opam repository as it lacks a \
             subdirectory named \"packages\""
            (String.maybe_quoted opam_repo_dir_path)
        ];
    validate_repo_file opam_repo_dir_path;
    { packages_dir_path }

  (* Return the path to an "opam" file describing a particular package
     (name and version) from this opam repository. *)
  let get_opam_file_path t opam_package =
    t.packages_dir_path
    / OpamPackage.name_to_string opam_package
    / OpamPackage.to_string opam_package
    / "opam"

  (* Reads an opam package definition from an "opam" file in this repository
     corresponding to a package (name and version). *)
  let load_opam_package t opam_package =
    let opam_file_path = get_opam_file_path t opam_package in
    if not (Sys.file_exists opam_file_path) then
      User_error.raise
        [ Pp.textf
            "Couldn't find package file for \"%s\". It was expected to be \
             located in %s but this file does not exist"
            (OpamPackage.to_string opam_package)
            (String.maybe_quoted opam_file_path)
        ];
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_file_path))
end

module Env = struct
  type t = OpamVariable.variable_contents OpamVariable.Map.t

  let empty : t = OpamVariable.Map.empty

  let global () : t =
    OpamGlobalState.with_ `Lock_none (fun global_state ->
        OpamVariable.Map.filter_map
          (fun _variable (contents, _description) -> Lazy.force contents)
          global_state.global_variables)

  let find_by_name (t : t) ~name =
    OpamVariable.Map.find_opt (OpamVariable.of_string name) t
end

module Local_repo_with_env = struct
  type t =
    { local_repo : Local_repo.t
    ; env : Env.t
    }
end

module Opam_solver = struct
  module type CONTEXT = Opam_0install.S.CONTEXT

  (* Helper functor which implements [CONTEXT] for [(L.t, R.t) Either.t] where
     [L] and [R] both implement [CONTEXT] *)
  module Context_either (L : CONTEXT) (R : CONTEXT) :
    CONTEXT with type t = (L.t, R.t) Either.t = struct
    type t = (L.t, R.t) Either.t

    type rejection = (L.rejection, R.rejection) Either.t

    let pp_rejection f = function
      | Left l -> L.pp_rejection f l
      | Right r -> R.pp_rejection f r

    let candidates t name =
      let convert_rejections ~f =
        List.map ~f:(fun (version, result) ->
            (version, Result.map_error ~f result))
      in
      match t with
      | Left l -> L.candidates l name |> convert_rejections ~f:Either.left
      | Right r -> R.candidates r name |> convert_rejections ~f:Either.right

    let user_restrictions = function
      | Left l -> L.user_restrictions l
      | Right r -> R.user_restrictions r

    let filter_deps = function
      | Left l -> L.filter_deps l
      | Right r -> R.filter_deps r
  end

  (* Helper functor which adds a set of local packages to a [CONTEXT] *)
  module Context_with_local_packages (Base_context : CONTEXT) : sig
    include CONTEXT

    val create :
         base_context:Base_context.t
      -> local_packages:OpamFile.OPAM.t OpamPackage.Name.Map.t
      -> t
  end = struct
    let local_package_default_version = OpamPackage.Version.of_string "LOCAL"

    type t =
      { base_context : Base_context.t
      ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
      }

    let create ~base_context ~local_packages = { base_context; local_packages }

    type rejection = Base_context.rejection

    let pp_rejection = Base_context.pp_rejection

    let candidates t name =
      match OpamPackage.Name.Map.find_opt name t.local_packages with
      | None -> Base_context.candidates t.base_context name
      | Some opam_file ->
        let version =
          Option.value opam_file.version ~default:local_package_default_version
        in
        [ (version, Ok opam_file) ]

    let user_restrictions t = Base_context.user_restrictions t.base_context

    let filter_deps t = Base_context.filter_deps t.base_context
  end

  (* A [CONTEXT] that can be based on an opam repository in a local directory
     or an opam repository taken from the current switch with an additional set
     of local packages *)
  module Context = struct
    module Dir_context = Opam_0install.Dir_context
    module Switch_context = Opam_0install.Switch_context
    include
      Context_with_local_packages (Context_either (Dir_context) (Switch_context))

    let create_dir_context ~local_repo_with_env ~local_packages =
      let { Local_repo_with_env.local_repo = { Local_repo.packages_dir_path }
          ; env
          } =
        local_repo_with_env
      in
      let env name = Env.find_by_name env ~name in
      let dir_context =
        Dir_context.create ~prefer_oldest:true
          ~constraints:OpamPackage.Name.Map.empty ~env packages_dir_path
      in
      create ~base_context:(Left dir_context) ~local_packages

    let create_switch_context ~switch_state ~local_packages =
      let switch_context =
        Switch_context.create ~constraints:OpamPackage.Name.Map.empty
          switch_state
      in
      create ~base_context:(Right switch_context) ~local_packages
  end
end

module Solver = Opam_0install.Solver.Make (Opam_solver.Context)

module Repo_state = struct
  type t =
    | Switch of OpamStateTypes.unlocked OpamStateTypes.switch_state
    | Local_repo_with_env of Local_repo_with_env.t

  let load_opam_package t opam_package =
    match t with
    | Switch switch_state -> OpamSwitchState.opam switch_state opam_package
    | Local_repo_with_env { local_repo; _ } ->
      Local_repo.load_opam_package local_repo opam_package

  let create_context t local_packages =
    match t with
    | Switch switch_state ->
      Opam_solver.Context.create_switch_context ~local_packages ~switch_state
    | Local_repo_with_env local_repo_with_env ->
      Opam_solver.Context.create_dir_context ~local_packages
        ~local_repo_with_env
end

module Repo_selection = struct
  type t =
    | Switch_with_name of string
    | Local_repo_with_env of Local_repo_with_env.t

  let switch_with_name switch_name = Switch_with_name switch_name

  let local_repo_with_env ~opam_repo_dir_path ~env =
    Local_repo_with_env
      { Local_repo_with_env.local_repo =
          Local_repo.of_opam_repo_dir_path opam_repo_dir_path
      ; env
      }

  (* [with_state ~f t] calls [f] on a [Repo_state.t] implied by [t] and
     returns the result. Don't let the [Repo_state.t] escape the function [f].
  *)
  let with_state ~f = function
    | Switch_with_name switch_name ->
      Global.with_switch_state ~switch_name ~f:(fun switch_state ->
          f (Repo_state.Switch switch_state))
    | Local_repo_with_env local_repo_with_env ->
      f (Repo_state.Local_repo_with_env local_repo_with_env)
end

module Summary = struct
  type t = { opam_packages_to_lock : OpamPackage.t list }

  let selected_packages_message t =
    match t.opam_packages_to_lock with
    | [] ->
      User_message.make
        [ Pp.tag User_message.Style.Success (Pp.text "No dependencies to lock")
        ]
    | opam_packages_to_lock ->
      User_message.make
        (Pp.tag User_message.Style.Success
           (Pp.text "Selected the following packages:")
        :: List.map opam_packages_to_lock ~f:(fun package ->
               Pp.text (OpamPackage.to_string package)))
end

let opam_package_to_lock_file_pkg ~repo_state ~local_packages opam_package =
  let name = OpamPackage.name opam_package in
  let version =
    OpamPackage.version opam_package |> OpamPackage.Version.to_string
  in
  let dev = OpamPackage.Name.Map.mem name local_packages in
  let info =
    { Lock_dir.Pkg_info.name =
        Package_name.of_string (OpamPackage.Name.to_string name)
    ; version
    ; dev
    ; source = None
    }
  in
  let opam_file =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | None -> Repo_state.load_opam_package repo_state opam_package
    | Some local_package -> local_package
  in
  (* This will collect all the atoms from the package's dependency formula regardless of conditions *)
  let deps =
    OpamFormula.fold_right
      (fun acc (name, _condition) -> name :: acc)
      [] opam_file.depends
    |> List.map ~f:(fun name ->
           Package_name.of_string (OpamPackage.Name.to_string name))
  in
  { Lock_dir.Pkg.build_command = None
  ; install_command = None
  ; deps
  ; info
  ; exported_env = []
  }

let solve_package_list local_packages context =
  let result =
    try
      (* [Solver.solve] returns [Error] when it's unable to find a solution to
         the dependencies, but can also raise exceptions, for example if opam
         is unable to parse an opam file in the package repository. To prevent
         an unexpected opam exception from crashing dune, we catch all
         exceptions raised by the solver and report them as [User_error]s
         instead. *)
      Solver.solve context (OpamPackage.Name.Map.keys local_packages)
    with
    | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format ->
      User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format) ]
    | unexpected_exn ->
      Code_error.raise "Unexpected exception raised while solving dependencies"
        [ ("exception", Exn.to_dyn unexpected_exn) ]
  in
  match result with
  | Error e -> User_error.raise [ Pp.text (Solver.diagnostics e) ]
  | Ok packages -> Solver.packages_of_result packages

let solve_lock_dir ~repo_selection local_packages =
  let is_local_package package =
    OpamPackage.Name.Map.mem (OpamPackage.name package) local_packages
  in
  Repo_selection.with_state repo_selection ~f:(fun repo_state ->
      let context = Repo_state.create_context repo_state local_packages in
      let opam_packages_to_lock =
        solve_package_list local_packages context
        (* don't include local packages in the lock dir *)
        |> List.filter ~f:(Fun.negate is_local_package)
      in
      let summary = { Summary.opam_packages_to_lock } in
      let lock_dir =
        match
          List.map opam_packages_to_lock ~f:(fun opam_package ->
              let pkg =
                opam_package_to_lock_file_pkg ~repo_state ~local_packages
                  opam_package
              in
              (pkg.info.name, pkg))
          |> Package_name.Map.of_list
        with
        | Error (name, _pkg1, _pkg2) ->
          Code_error.raise
            (sprintf "Solver selected multiple packages named \"%s\""
               (Package_name.to_string name))
            []
        | Ok pkgs_by_name -> Lock_dir.create_latest_version pkgs_by_name
      in
      (summary, lock_dir))
