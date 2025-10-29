open Import

include struct
  open Dune_pkg
  module Solver_env = Solver_env
  module Opam_repo = Opam_repo
  module Local_package = Local_package
  module Resolved_package = Resolved_package
  module Version_preference = Version_preference
  module Package_universe = Package_universe
  module Package_dependency = Package_dependency
  module Opam_solver = Opam_solver
  module Sys_poll = Sys_poll
end

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : 'path
    ; packages : Local_package.t Package.Name.Map.t
    ; repos : Opam_repo.t list
    ; solver_env_from_context : Solver_env.t
    ; unset_solver_vars : Package_variable_name.Set.t
    ; constraints : Package_dependency.t list
    ; selected_depopts : Package.Name.t list
    ; pins : Resolved_package.t Package.Name.Map.t
    ; version_preference : Version_preference.t
    }

  let name = "lock"
  let version = 1
  let bimap t f g = { t with lock_dir = f t.lock_dir; target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode
        { target
        ; lock_dir
        ; packages
        ; repos
        ; solver_env_from_context
        ; unset_solver_vars
        ; constraints
        ; selected_depopts
        ; pins
        ; version_preference
        }
        encode_path
        encode_target
    =
    Sexp.record
      [ "target", encode_target target
      ; "lock_dir", encode_path lock_dir
      ; ( "packages"
        , match Package_universe.dependency_digest packages with
          | None -> Atom "no packages"
          | Some hash ->
            List [ Atom "hash"; Atom (Local_package.Dependency_hash.to_string hash) ] )
      ; ( "repos"
        , List
            (List.map repos ~f:(fun repo ->
               Sexp.Atom (Opam_repo.content_digest repo |> Dune_digest.to_string))) )
      ; ( "solver_env_from_context"
        , Atom
            (Dune_digest.Feed.compute_digest
               Solver_env.digest_feed
               solver_env_from_context
             |> Dune_digest.to_string) )
      ; ( "unset_solver_vars"
        , List
            (Package_variable_name.Set.to_list unset_solver_vars
             |> List.sort ~compare:Package_variable_name.compare
             |> List.map ~f:(fun var -> Sexp.Atom (Package_variable_name.to_string var)))
        )
      ; ( "constraints"
        , List
            (List.sort constraints ~compare:(fun a b ->
               Dune_lang.Package_name.compare
                 a.Package_dependency.name
                 b.Package_dependency.name)
             |> List.map ~f:(fun { Package_dependency.name; constraint_ } ->
               let name = Dune_lang.Package_name.to_string name in
               let constraint_ =
                 match constraint_ with
                 | None -> "no constraints"
                 | Some c -> Package_dependency.Constraint.to_dyn c |> Dyn.to_string
               in
               Sexp.List [ Sexp.Atom name; Sexp.Atom constraint_ ])) )
      ; ( "selected_depopts"
        , List
            (List.sort selected_depopts ~compare:Dune_lang.Package_name.compare
             |> List.map ~f:(fun pkg_name ->
               Sexp.Atom (Dune_lang.Package_name.to_string pkg_name))) )
      ; ( "pins"
        , List
            (Dune_lang.Package_name.Map.to_list pins
             |> List.sort ~compare:(fun (a, _) (b, _) ->
               Dune_lang.Package_name.compare a b)
             |> List.map ~f:(fun (pkg_name, resolved_pkg) ->
               let name = Dune_lang.Package_name.to_string pkg_name in
               let digest =
                 Resolved_package.digest resolved_pkg |> Dune_digest.to_string
               in
               Sexp.List [ Sexp.Atom name; Sexp.Atom digest ])) )
      ; ( "version_preference"
        , Atom
            (match version_preference with
             | Oldest -> "oldest"
             | Newest -> "newest") )
      ]
  ;;

  let action
        { target
        ; lock_dir = _
        ; packages
        ; repos
        ; solver_env_from_context
        ; unset_solver_vars
        ; constraints
        ; selected_depopts
        ; pins
        ; version_preference
        }
        ~ectx:_
        ~eenv:{ Action.Ext.Exec.env; _ }
    =
    let open Fiber.O in
    let* () = Fiber.return () in
    let local_packages = Package.Name.Map.map packages ~f:Local_package.for_solver in
    (* Whether or not the lock directory we are creating is portable or not
       doesn't concern us. We therefore set it as non-portable. *)
    let portable_lock_dir = false in
    let* solver_env =
      let open Fiber.O in
      let+ solver_env_from_current_system =
        Sys_poll.make ~path:(Env_path.path env) |> Sys_poll.solver_env_from_current_system
      in
      let solver_env =
        [ solver_env_from_current_system; solver_env_from_context ]
        |> List.fold_left ~init:Solver_env.with_defaults ~f:Solver_env.extend
      in
      Solver_env.unset_multi solver_env unset_solver_vars
    in
    let* solver_result =
      Opam_solver.solve_lock_dir
        solver_env
        version_preference
        repos
        ~pins
        ~local_packages
        ~constraints
        ~selected_depopts
        ~portable_lock_dir
    in
    match solver_result with
    | Error (`Manifest_error diagnostic) -> raise (User_error.E diagnostic)
    | Error (`Solve_error diagnostic) -> User_error.raise [ diagnostic ]
    | Ok { pinned_packages; files; lock_dir; _ } ->
      let lock_dir_path = Path.build target in
      let+ lock_dir =
        Dune_pkg.Lock_dir.compute_missing_checksums ~pinned_packages lock_dir
      in
      Dune_pkg.Lock_dir.Write_disk.prepare
        ~portable_lock_dir
        ~lock_dir_path
        ~files
        lock_dir
      |> Dune_pkg.Lock_dir.Write_disk.commit
  ;;
end

module A = Action_ext.Make (Spec)

let action
      ~target
      ~lock_dir
      ~packages
      ~repos
      ~solver_env_from_context
      ~unset_solver_vars
      ~constraints
      ~selected_depopts
      ~pins
      ~version_preference
  =
  A.action
    { Spec.target
    ; lock_dir
    ; packages
    ; repos
    ; solver_env_from_context
    ; unset_solver_vars
    ; constraints
    ; selected_depopts
    ; pins
    ; version_preference
    }
;;
