open Stdune
module Package_name = Dune_lang.Package_name

module Repo = struct
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

(* A custom solver context based on [Opam_0install.Dir_context] with a set
   of local packages (ie. the packages defined in the current project).
   When looking up a package during solving, the local packages are
   searched before falling back to packages defined in a directory in the
   style of opam-repository. *)
module Solver_context = struct
  module Dir_context = Opam_0install.Dir_context

  (* Version to use for local packages with no version number *)
  let local_package_default_version = OpamPackage.Version.of_string "LOCAL"

  type t =
    { dir_context : Dir_context.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    }

  type rejection = Dir_context.rejection

  let pp_rejection = Dir_context.pp_rejection

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.local_packages with
    | None -> Dir_context.candidates t.dir_context name
    | Some opam_file ->
      let version =
        Option.value opam_file.version ~default:local_package_default_version
      in
      [ (version, Ok opam_file) ]

  let user_restrictions t = Dir_context.user_restrictions t.dir_context

  let filter_deps t = Dir_context.filter_deps t.dir_context

  let create ~env ~repo ~local_packages =
    let env name = Env.find_by_name env ~name in
    let { Repo.packages_dir_path } = repo in
    let dir_context =
      Dir_context.create ~prefer_oldest:true
        ~constraints:OpamPackage.Name.Map.empty ~env packages_dir_path
    in
    { dir_context; local_packages }
end

module Solver = Opam_0install.Solver.Make (Solver_context)

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

let opam_package_to_lock_file_pkg ~repo ~local_packages ~lock_dir_path
    opam_package =
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
    | None -> Repo.load_opam_package repo opam_package
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
  ; lock_dir = lock_dir_path
  ; exported_env = []
  }

let solve_package_list local_packages ~env ~repo =
  let context = Solver_context.create ~env ~repo ~local_packages in
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

let solve_lock_dir ~env ~repo ~lock_dir_path local_packages =
  let is_local_package package =
    OpamPackage.Name.Map.mem (OpamPackage.name package) local_packages
  in
  let opam_packages_to_lock =
    solve_package_list local_packages ~env ~repo
    (* don't include local packages in the lock dir *)
    |> List.filter ~f:(Fun.negate is_local_package)
  in
  let summary = { Summary.opam_packages_to_lock } in
  let lock_dir =
    List.map opam_packages_to_lock ~f:(fun opam_package ->
        let pkg =
          opam_package_to_lock_file_pkg ~repo ~local_packages ~lock_dir_path
            opam_package
        in
        (pkg.info.name, pkg))
    |> Package_name.Map.of_list
    |> function
    | Error (name, _pkg1, _pkg2) ->
      Code_error.raise
        (sprintf "Solver selected multiple packages named \"%s\""
           (Package_name.to_string name))
        []
    | Ok pkgs_by_name -> Lock_dir.create_latest_version pkgs_by_name
  in
  (summary, lock_dir)
