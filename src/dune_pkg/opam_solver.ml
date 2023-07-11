open Stdune
module Package_name = Dune_lang.Package_name

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper module for working with [OpamTypes.filtered_formula] *)
module Filtered_formula : sig
  open OpamTypes

  (** Update a filtered formula setting the "with-test" and "with-doc" variables
      to true. *)
  val with_flags : Solver_env.Flag.Set.t -> filtered_formula -> filtered_formula
end = struct
  open OpamTypes

  (* [map_formula_filters_bottom_up t ~f] calls [f] on each node of each filter
     in t, bottom-up *)
  let map_filters_bottom_up ~f =
    OpamFilter.gen_filter_formula
      (OpamFormula.partial_eval (function
        | Filter flt -> `Formula (Atom (Filter (OpamFilter.map_up f flt)))
        | Constraint _ as constraint_ -> `Formula (Atom constraint_)))

  (* update a filtered formula replacing instances of a given variable with a given boolean value *)
  let resolve_simple_bool_variable filtered_formula variable_to_resolve value =
    map_filters_bottom_up filtered_formula ~f:(function
      | FIdent ([], variable, None) as filter ->
        if OpamVariable.equal variable variable_to_resolve then FBool value
        else filter
      | other -> other)

  let with_flags flags filtered_formula =
    Solver_env.Flag.Set.fold flags ~init:filtered_formula ~f:(fun flag acc ->
        resolve_simple_bool_variable acc
          (OpamVariable.of_string (Solver_env.Flag.to_string flag))
          true)
end

(* Helper functor which adds a set of local packages to a [CONTEXT] *)
module Context_with_local_packages (Base_context : CONTEXT) : sig
  include CONTEXT

  val create :
       base_context:Base_context.t
    -> local_packages:OpamFile.OPAM.t OpamPackage.Name.Map.t
    -> solver_env:Solver_env.t
    -> t
end = struct
  let local_package_default_version =
    OpamPackage.Version.of_string Lock_dir.Pkg_info.default_version

  type t =
    { base_context : Base_context.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    ; solver_env : Solver_env.t
    }

  let create ~base_context ~local_packages ~solver_env =
    { base_context; local_packages; solver_env }

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

  (* Override [filter_deps] so that local packages get test and doc dependencies *)
  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.Name.Map.mem (OpamPackage.name package) t.local_packages
    in
    let filtered_formula =
      if package_is_local then
        Filtered_formula.with_flags t.solver_env.flags filtered_formula
      else filtered_formula
    in
    Base_context.filter_deps t.base_context package filtered_formula
end

module Context_for_dune = struct
  module Dir_context = Opam_0install.Dir_context
  include Context_with_local_packages (Dir_context)

  let prefer_oldest = function
    | Version_preference.Oldest -> true
    | Newest -> false

  let dir_context_env_of_solver_env { Solver_env.flags; sys } variable_name =
    match Solver_env.Flag.of_string_opt variable_name with
    | Some flag -> Some (OpamVariable.B (Solver_env.Flag.Set.mem flags flag))
    | None ->
      Solver_env.Sys_var.of_string_opt variable_name
      |> Option.bind ~f:(fun sys_var ->
             Solver_env.Sys_var.Bindings.get sys sys_var
             |> Option.map ~f:OpamVariable.string)

  let create_dir_context ~solver_env ~repo ~local_packages ~version_preference =
    let env = dir_context_env_of_solver_env solver_env in
    let packages_dir_path = Opam_repo.packages_dir_path repo in
    let dir_context =
      Dir_context.create
        ~prefer_oldest:(prefer_oldest version_preference)
        ~constraints:OpamPackage.Name.Map.empty ~env packages_dir_path
    in
    create ~base_context:dir_context ~local_packages ~solver_env
end

module Solver = Opam_0install.Solver.Make (Context_for_dune)

module Summary = struct
  type t = { opam_packages_to_lock : OpamPackage.t list }

  let selected_packages_message t ~lock_dir_path =
    let parts =
      match t.opam_packages_to_lock with
      | [] ->
        [ Pp.tag User_message.Style.Success
            (Pp.text "(no dependencies to lock)")
        ]
      | opam_packages_to_lock ->
        List.map opam_packages_to_lock ~f:(fun package ->
            Pp.text (OpamPackage.to_string package))
    in
    User_message.make
      (Pp.textf "Solution for %s:"
         (Path.Source.to_string_maybe_quoted lock_dir_path)
      :: parts)
end

let opam_package_to_lock_file_pkg ~repo ~local_packages opam_package =
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
    ; extra_sources = []
    }
  in
  let opam_file =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | None -> Opam_repo.load_opam_package repo opam_package
    | Some local_package -> local_package
  in
  (* This will collect all the atoms from the package's dependency formula regardless of conditions *)
  let deps =
    OpamFormula.fold_right
      (fun acc (name, _condition) -> name :: acc)
      [] opam_file.depends
    |> List.map ~f:(fun name ->
           (Loc.none, Package_name.of_string (OpamPackage.Name.to_string name)))
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

let solve_lock_dir ~solver_env ~version_preference ~repo local_packages =
  let is_local_package package =
    OpamPackage.Name.Map.mem (OpamPackage.name package) local_packages
  in
  let context =
    Context_for_dune.create_dir_context ~solver_env ~repo ~local_packages
      ~version_preference
  in
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
            opam_package_to_lock_file_pkg ~repo ~local_packages opam_package
          in
          (pkg.info.name, pkg))
      |> Package_name.Map.of_list
    with
    | Error (name, _pkg1, _pkg2) ->
      Code_error.raise
        (sprintf "Solver selected multiple packages named \"%s\""
           (Package_name.to_string name))
        []
    | Ok pkgs_by_name -> Lock_dir.create_latest_version pkgs_by_name ~ocaml:None
  in
  (summary, lock_dir)
