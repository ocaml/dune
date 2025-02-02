open Import
open Fiber.O

let add_self_to_filter_env package env variable =
  match OpamVariable.Full.scope variable with
  | Self | Package _ -> env variable
  | Global ->
    let var_name = Package_variable_name.of_opam (OpamVariable.Full.variable variable) in
    if Package_variable_name.(equal var_name name)
    then Some (OpamVariable.S (OpamPackage.Name.to_string (OpamPackage.name package)))
    else if Package_variable_name.(equal var_name version)
    then Some (S (OpamPackage.Version.to_string (OpamPackage.version package)))
    else env variable
;;

module Priority = struct
  (* A priority defines a package's position in the list of candidates
     fed to the solver. Any change to package selection should be reflected in
     this priority rather than implemented in an ad-hoc manner *)
  type t =
    { (* We don't really need this field, since we filter avoid-version
         packages. If this changes, we still prefer packages
         with [avoid-version: false] *)
      avoid : bool
    ; version : OpamPackage.Version.t
    }

  let compare_version =
    let ord x y = OpamPackage.Version.compare x y |> Ordering.of_int in
    fun (pref : Version_preference.t) x y ->
      match pref with
      | Oldest -> ord x y
      | Newest -> ord y x
  ;;

  let compare pref t { avoid; version } =
    Tuple.T2.compare
      Bool.compare
      (compare_version pref)
      (t.avoid, t.version)
      (avoid, version)
  ;;

  let make (package : OpamFile.OPAM.t) =
    let avoid = List.mem package.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
    let version = OpamFile.OPAM.package package |> OpamPackage.version in
    { version; avoid }
  ;;
end

module Context = struct
  type rejection =
    | (* TODO proper error messages for packages skipped via avoid-version *)
      Unavailable
    | Avoid_version

  let local_package_default_version =
    Package_version.to_opam_package_version Lock_dir.Pkg_info.default_version
  ;;

  type candidates =
    { resolved : Resolved_package.t OpamPackage.Version.Map.t
    ; available : (OpamTypes.version * (OpamFile.OPAM.t, rejection) result) list
    }

  type t =
    { repos : Opam_repo.t list
    ; version_preference : Version_preference.t
    ; pinned_packages : Resolved_package.t Package_name.Map.t
    ; local_packages : OpamFile.OPAM.t Package_name.Map.t
    ; solver_env : Solver_env.t
    ; dune_version : OpamPackage.Version.t
    ; stats_updater : Solver_stats.Updater.t
    ; candidates_cache : (Package_name.t, candidates) Fiber_cache.t
    ; (* The solver can call this function several times on the same package.
         If the package contains an invalid "available" filter we want to print a
         warning, but only once per package. This field will keep track of the
         packages for which we've printed a warning. *)
      available_cache : (OpamPackage.t, bool) Table.t
    ; constraints : OpamTypes.filtered_formula Package_name.Map.t
    ; (* Number of versions of each package whose opam files were read from
         disk while solving. Used to report performance statistics. *)
      expanded_packages : (Package_name.t, int) Table.t
    }

  let create
    ~pinned_packages
    ~solver_env
    ~repos
    ~local_packages
    ~version_preference
    ~stats_updater
    ~constraints
    =
    let candidates_cache = Fiber_cache.create (module Package_name) in
    let constraints =
      List.map constraints ~f:(fun (constraint_ : Package_dependency.t) ->
        constraint_.name, constraint_)
      |> Package_name.Map.of_list_multi
      |> Package_name.Map.map ~f:Package_dependency.list_to_opam_filtered_formula
    in
    let available_cache =
      Table.create
        (module struct
          include OpamPackage

          let to_dyn = Opam_dyn.package
        end)
        1
    in
    let expanded_packages =
      Table.create
        (module struct
          include Package_name

          let to_dyn = Package_name.to_dyn
        end)
        1
    in
    { repos
    ; version_preference
    ; local_packages
    ; pinned_packages
    ; solver_env
    ; dune_version = Dune_dep.version
    ; stats_updater
    ; candidates_cache
    ; available_cache
    ; constraints
    ; expanded_packages
    }
  ;;

  let pp_rejection = function
    | Unavailable -> Pp.paragraph "Availability condition not satisfied"
    | Avoid_version -> Pp.paragraph "Package is excluded by avoid-version"
  ;;

  let eval_to_bool (filter : OpamTypes.filter) : (bool, [> `Not_a_bool of string ]) result
    =
    try Ok (OpamFilter.eval_to_bool ~default:false (Fun.const None) filter) with
    | Invalid_argument msg -> Error (`Not_a_bool msg)
  ;;

  let is_opam_available t opam =
    let package = OpamFile.OPAM.package opam in
    Table.find_or_add t.available_cache package ~f:(fun (_ : OpamPackage.t) ->
      let available = OpamFile.OPAM.available opam in
      match
        OpamFilter.partial_eval
          (add_self_to_filter_env
             package
             (Solver_stats.Updater.wrap_env
                t.stats_updater
                (Solver_env.to_env t.solver_env)))
          available
        |> eval_to_bool
      with
      | Ok available -> available
      | Error (`Not_a_bool msg) ->
        (let package_string = OpamFile.OPAM.package opam |> OpamPackage.to_string in
         let available_string = OpamFilter.to_string available in
         User_warning.emit
           [ Pp.textf
               "Ignoring package %s as its \"available\" filter can't be resolved to a \
                boolean value."
               package_string
           ; Pp.textf "available: %s" available_string
           ; Pp.text msg
           ]);
        false)
  ;;

  let available_or_error t opam_file =
    (* The CONTEXT interface doesn't give us a way to report this type of
       error and there's not enough context to give a helpful error message
       so just tell opam_0install that there are no versions of this
       package available (technically true) and let it produce the error
       message. *)
    if is_opam_available t opam_file then Ok opam_file else Error Unavailable
  ;;

  let pinned_candidate t resolved_package =
    let version = Resolved_package.package resolved_package |> OpamPackage.version in
    let available =
      (* We don't respect avoid-version for pinned packages. This is
         intentional. *)
      [ version, Resolved_package.opam_file resolved_package |> available_or_error t ]
    in
    let resolved = OpamPackage.Version.Map.singleton version resolved_package in
    { available; resolved }
  ;;

  let repo_candidate t name =
    let+ resolved = Opam_repo.load_all_versions t.repos name in
    Table.add_exn
      t.expanded_packages
      (Package_name.of_opam_package_name name)
      (OpamPackage.Version.Map.cardinal resolved);
    let available =
      OpamPackage.Version.Map.values resolved
      |> List.map ~f:(fun p -> p, Priority.make (Resolved_package.opam_file p))
      (* Note that although the packages are taken from a map,
         explicitly sorting them is still necessary. This sort applies
         the configured version preference and also allows the solver to
         prefer versions without the avoid-version flag set. *)
      |> List.sort ~compare:(fun (_, x) (_, y) ->
        Priority.compare t.version_preference x y)
      |> List.map ~f:(fun (resolved_package, (priority : Priority.t)) ->
        let opam_file = Resolved_package.opam_file resolved_package in
        let opam_file_result =
          if priority.avoid then Error Avoid_version else available_or_error t opam_file
        in
        OpamFile.OPAM.version opam_file, opam_file_result)
    in
    { available; resolved }
  ;;

  let candidates t name =
    let* () = Fiber.return () in
    let key = Package_name.of_opam_package_name name in
    match Package_name.Map.find t.local_packages key with
    | Some local_package ->
      let version =
        Option.value local_package.version ~default:local_package_default_version
      in
      Fiber.return [ version, Ok local_package ]
    | None ->
      let+ res =
        Fiber_cache.find_or_add t.candidates_cache key ~f:(fun () ->
          match Package_name.Map.find t.pinned_packages key with
          | Some resolved_package -> Fiber.return (pinned_candidate t resolved_package)
          | None -> repo_candidate t name)
      in
      res.available
  ;;

  let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
    =
    fun t pkg ->
    if Package_name.equal Dune_dep.name (Package_name.of_opam_package_name pkg)
    then Some (`Eq, t.dune_version)
    else None
  ;;

  let filter_deps t package filtered_formula =
    let name = OpamPackage.name package |> Package_name.of_opam_package_name in
    (* Add additional constraints to the formula. This works in two steps.
       First identify all the additional constraints applied to packages which
       appear in the current package's dependency formula. Then each additional
       constnraint is and-ed with the current package's dependency formula. *)
    let filtered_formula =
      OpamFormula.fold_left
        (fun additional_formulae (pkg, _) ->
          let name = Package_name.of_opam_package_name pkg in
          match Package_name.Map.find t.constraints name with
          | None -> additional_formulae
          | Some additional -> additional :: additional_formulae)
        []
        filtered_formula
      |> List.fold_left ~init:filtered_formula ~f:(fun additional acc ->
        OpamFormula.And (acc, additional))
    in
    let package_is_local = Package_name.Map.mem t.local_packages name in
    Resolve_opam_formula.apply_filter
      (add_self_to_filter_env
         package
         (Solver_stats.Updater.wrap_env t.stats_updater (Solver_env.to_env t.solver_env)))
      ~with_test:package_is_local
      filtered_formula
  ;;

  let count_expanded_packages t = Table.fold t.expanded_packages ~init:0 ~f:( + )
end

module Solver = struct
  open Pp.O

  module Dep_kind = struct
    type t =
      | Ensure
      | Prevent
  end

  (* Copyright (c) 2020 Thomas Leonard <talex5@gmail.com>

     Permission to use, copy, modify, and distribute this software for any
     purpose with or without fee is hereby granted, provided that the above
     copyright notice and this permission notice appear in all copies.

     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
     WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
     ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
     WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
     OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  *)
  module Input = struct
    (* Note: [OpamFormula.neg] doesn't work in the [Empty] case, so we just
       record whether to negate the result here. *)
    module Restriction = struct
      type t =
        { kind : Dep_kind.t
        ; expr : OpamFormula.version_formula
        }

      let string_of_version_formula =
        let string_of_op =
          let pos =
            { OpamParserTypes.FullPos.filename = ""; start = 0, 0; stop = 0, 0 }
          in
          fun pelem -> OpamPrinter.FullPos.relop { pelem; pos }
        in
        OpamFormula.string_of_formula (fun (rel, v) ->
          Printf.sprintf "%s %s" (string_of_op rel) (OpamPackage.Version.to_string v))
      ;;

      let to_string = function
        | { kind = Prevent; expr = OpamFormula.Empty } -> "conflict with all versions"
        | { kind = Prevent; expr } ->
          Format.sprintf "not(%s)" (string_of_version_formula expr)
        | { kind = Ensure; expr } -> string_of_version_formula expr
      ;;
    end

    module Virtual_id = Id.Make ()

    module Rank : sig
      type t
      type assign

      val compare : t -> t -> Ordering.t
      val bottom : t
      val of_int : int -> t
      val next : assign -> t
      val assign : unit -> assign
    end = struct
      type t = int

      let bottom = -1
      let of_int x = x
      let compare = Int.compare

      type assign = int ref

      let assign () = ref 0

      let next t =
        let res = !t in
        incr t;
        res
      ;;
    end

    type role =
      | Real of OpamPackage.Name.t
      | Virtual of Virtual_id.t * impl list

    and real_impl =
      { pkg : OpamPackage.t
      ; conflict_class : OpamPackage.Name.t list
      ; requires : dependency list Lazy.t
      }

    and dependency =
      { drole : role
      ; importance : Dep_kind.t
      ; restrictions : Restriction.t list
      }

    and impl =
      | RealImpl of real_impl (* An implementation is usually an opam package *)
      | VirtualImpl of Rank.t * dependency list (* (rank just for sorting) *)
      | Reject of OpamPackage.t
      | Dummy (* Used for diagnostics *)

    let rec pp_version = function
      | RealImpl impl ->
        Pp.text (OpamPackage.Version.to_string (OpamPackage.version impl.pkg))
      | Reject pkg -> Pp.text (OpamPackage.version_to_string pkg)
      | VirtualImpl (_i, deps) ->
        Pp.concat_map ~sep:(Pp.char '&') deps ~f:(fun d -> pp_role d.drole)
      | Dummy -> Pp.text "(no version)"

    and pp_impl = function
      | RealImpl impl -> Pp.text (OpamPackage.to_string impl.pkg)
      | Reject pkg -> Pp.text (OpamPackage.to_string pkg)
      | VirtualImpl _ as x -> pp_version x
      | Dummy -> Pp.text "(no solution found)"

    and pp_role = function
      | Real name -> Pp.text (OpamPackage.Name.to_string name)
      | Virtual (_, impls) -> Pp.concat_map ~sep:(Pp.char '|') impls ~f:pp_impl
    ;;

    let pp_impl_long = pp_impl

    module Role = struct
      module T = struct
        type t = role

        let compare a b =
          match a, b with
          | Real a, Real b -> Ordering.of_int (OpamPackage.Name.compare a b)
          | Virtual (a, _), Virtual (b, _) -> Virtual_id.compare a b
          | Real _, Virtual _ -> Lt
          | Virtual _, Real _ -> Gt
        ;;

        let to_dyn = Dyn.opaque
      end

      include T

      let equal x y = Ordering.is_eq (compare x y)
      let hash = Poly.hash

      let user_restrictions t context =
        match t with
        | Virtual _ -> None
        | Real role ->
          Context.user_restrictions context role
          |> Option.map ~f:(fun f ->
            { Restriction.kind = Ensure; expr = OpamFormula.Atom f })
      ;;

      let pp = pp_role

      let rejects role context =
        match role with
        | Virtual _ -> Fiber.return ([], [])
        | Real role ->
          let+ rejects =
            Context.candidates context role
            >>| List.filter_map ~f:(function
              | _, Ok _ -> None
              | version, Error reason ->
                let pkg = OpamPackage.create role version in
                Some (Reject pkg, reason))
          in
          let notes = [] in
          rejects, notes
      ;;

      module Map = Map.Make (T)
    end

    module Impl = struct
      type t = impl

      let pp = pp_impl

      let requires _ = function
        | Dummy | Reject _ -> []
        | VirtualImpl (_, deps) -> deps
        | RealImpl impl -> Lazy.force impl.requires
      ;;

      let conflict_class = function
        | RealImpl impl -> impl.conflict_class
        | VirtualImpl _ -> []
        | Dummy | Reject _ -> []
      ;;

      let version = function
        | RealImpl impl -> Some impl.pkg
        | Reject pkg -> Some pkg
        | VirtualImpl _ -> None
        | Dummy -> None
      ;;

      let compare_version a b =
        match a, b with
        | RealImpl a, RealImpl b -> Ordering.of_int (OpamPackage.compare a.pkg b.pkg)
        | RealImpl _, _ -> Gt
        | _, RealImpl _ -> Lt
        | VirtualImpl (ia, _), VirtualImpl (ib, _) -> Rank.compare ia ib
        | VirtualImpl _, _ -> Gt
        | _, VirtualImpl _ -> Lt
        | Reject a, Reject b -> Ordering.of_int (OpamPackage.compare a b)
        | Reject _, _ -> Gt
        | _, Reject _ -> Lt
        | Dummy, Dummy -> Eq
      ;;
    end

    let virtual_role impls =
      let impls =
        List.mapi impls ~f:(fun i ->
            function
            | VirtualImpl (_, x) -> VirtualImpl (Rank.of_int i, x)
            | x -> x)
      in
      Virtual (Virtual_id.gen (), impls)
    ;;

    (* Opam uses conflicts, e.g.
       conflicts if X {> 1} OR Y {< 1 OR > 2}
     whereas 0install uses restricts, e.g.
       restrict to X {<= 1} AND Y {>= 1 AND <= 2}

     Warning: [OpamFormula.neg _ Empty = Empty], so does NOT reverse the result in this case.
     For empty conflicts this is fine (don't conflict with anything, just like an empty depends
     list). But for the version expressions inside, it's wrong: a conflict with no expression
     conflicts with all versions and should restrict the choice to nothing, not to everything.
     So, we just tag the formula as [Prevent] instead of negating it. *)
    let prevent f =
      OpamFormula.neg Fun.id f
      |> OpamFormula.map (fun (a, expr) ->
        OpamFormula.Atom (a, [ { Restriction.kind = Prevent; expr } ]))
    ;;

    let ensure =
      OpamFormula.map (fun (name, vexpr) ->
        let rlist =
          match vexpr with
          | OpamFormula.Empty -> []
          | r -> [ { Restriction.kind = Ensure; expr = r } ]
        in
        OpamFormula.Atom (name, rlist))
    ;;

    (* Turn an opam dependency formula into a 0install list of dependencies. *)
    let list_deps ~importance ~rank deps =
      let rec aux (formula : _ OpamTypes.generic_formula) =
        match formula with
        | Empty -> []
        | Atom (name, restrictions) -> [ { drole = Real name; restrictions; importance } ]
        | Block x -> aux x
        | And (x, y) -> aux x @ aux y
        | Or _ as o ->
          let impls = group_ors o in
          let drole = virtual_role impls in
          (* Essential because we must apply a restriction, even if its
             components are only restrictions. *)
          [ { drole; restrictions = []; importance = Ensure } ]
      and group_ors = function
        | Or (x, y) -> group_ors x @ group_ors y
        | expr -> [ VirtualImpl (Rank.next rank, aux expr) ]
      in
      aux deps
    ;;

    (* Get all the candidates for a role. *)
    let implementations role context =
      match role with
      | Virtual (_, impls) -> Fiber.return impls
      | Real role ->
        Context.candidates context role
        >>| List.filter_map ~f:(function
          | _, Error _rejection -> None
          | version, Ok opam ->
            let pkg = OpamPackage.create role version in
            (* Note: we ignore depopts here: see opam/doc/design/depopts-and-features *)
            let requires =
              lazy
                (let rank = Rank.assign () in
                 let make_deps importance xform get =
                   get opam
                   |> Context.filter_deps context pkg
                   |> xform
                   |> list_deps ~importance ~rank
                 in
                 make_deps Ensure ensure OpamFile.OPAM.depends
                 @ make_deps Prevent prevent OpamFile.OPAM.conflicts)
            in
            let conflict_class = OpamFile.OPAM.conflict_class opam in
            Some (RealImpl { pkg; requires; conflict_class }))
    ;;

    let meets_restriction impl { Restriction.kind; expr } =
      match impl with
      | Dummy -> true
      | VirtualImpl _ -> assert false (* Can't constrain version of a virtual impl! *)
      | Reject _ -> false
      | RealImpl impl ->
        let result =
          OpamFormula.check_version_formula expr (OpamPackage.version impl.pkg)
        in
        (match kind with
         | Ensure -> result
         | Prevent -> not result)
    ;;
  end

  module Solver = struct
    (* Copyright (C) 2013, Thomas Leonard
     *See the README file for details, or visit http://0install.net.
     *)
    module Sat = Sat.Make (Input.Impl)

    type decision_state =
      (* The next candidate to try *)
      | Undecided of Sat.lit
      (* The dependencies to check next *)
      | Selected of Input.dependency list
      | Unselected

    type selection =
      { impl : Input.Impl.t (** The implementation chosen to fill the role *)
      ; var : Sat.lit
      }

    module Candidates = struct
      type t =
        { role : Input.Role.t
        ; clause : Sat.at_most_one_clause option
        ; vars : selection list
        }

      let selected t =
        let open Option.O in
        let+ lit = t.clause >>= Sat.get_selected in
        let impl = Sat.get_user_data_for_lit lit in
        { var = lit; impl }
      ;;

      let state t =
        match t.clause with
        | None -> Unselected (* There were never any candidates *)
        | Some clause ->
          (match Sat.get_selected clause with
           | Some lit ->
             (* We've already chosen which <implementation> to use. Follow dependencies. *)
             let impl = Sat.get_user_data_for_lit lit in
             Selected (Input.Impl.requires t.role impl)
           | None ->
             (match Sat.get_best_undecided clause with
              | Some lit -> Undecided lit
              | None -> Unselected (* No remaining candidates, and none was chosen. *)))
      ;;

      (* Add the implementations of an interface to the implementation cache
         (called the first time we visit it). *)
      let make_impl_clause sat context ~dummy_impl role =
        (* Insert dummy_impl (last) if we're trying to diagnose a problem. *)
        let+ impls =
          let+ impls = Input.implementations role context in
          (match dummy_impl with
           | None -> impls
           | Some dummy_impl -> impls @ [ dummy_impl ])
          |> List.map ~f:(fun impl ->
            let var = Sat.add_variable sat impl in
            { impl; var })
        in
        let clause =
          let impl_clause =
            match impls with
            | [] -> None
            | _ :: _ -> Some (Sat.at_most_one (List.map impls ~f:(fun s -> s.var)))
          in
          { role; clause = impl_clause; vars = impls }
        in
        clause, impls
      ;;
    end

    module Conflict_classes = struct
      type t = { mutable groups : Sat.lit list ref OpamPackage.Name.Map.t }

      let create () = { groups = OpamPackage.Name.Map.empty }

      let var t name =
        match OpamPackage.Name.Map.find_opt name t.groups with
        | Some v -> v
        | None ->
          let v = ref [] in
          t.groups <- OpamPackage.Name.Map.add name v t.groups;
          v
      ;;

      (* Add [impl] to its conflict groups, if any. *)
      let process t impl_var impl =
        Input.Impl.conflict_class impl
        |> List.iter ~f:(fun name ->
          let impls = var t name in
          impls := impl_var :: !impls)
      ;;

      (* Call this at the end to add the final clause with all discovered groups.
         [t] must not be used after this. *)
      let seal t =
        OpamPackage.Name.Map.iter
          (fun _ impls ->
            match !impls with
            | _ :: _ :: _ ->
              let (_ : Sat.at_most_one_clause) = Sat.at_most_one !impls in
              ()
            | _ -> ())
          t.groups
      ;;
    end

    (* Starting from [root_req], explore all the feeds and implementations we
       might need, adding all of them to [sat_problem]. *)
    let build_problem context root_req sat ~dummy_impl =
      (* For each (iface, source) we have a list of implementations. *)
      let impl_cache = ref Input.Role.Map.empty in
      let conflict_classes = Conflict_classes.create () in
      let+ () =
        let rec lookup_impl expand_deps role =
          match Input.Role.Map.find !impl_cache role with
          | Some s -> Fiber.return s
          | None ->
            let* clause, impls =
              Candidates.make_impl_clause sat context ~dummy_impl role
            in
            impl_cache := Input.Role.Map.set !impl_cache role clause;
            let+ () =
              Fiber.sequential_iter impls ~f:(fun { var = impl_var; impl } ->
                Conflict_classes.process conflict_classes impl_var impl;
                match expand_deps with
                | `No_expand -> Fiber.return ()
                | `Expand_and_collect_conflicts deferred ->
                  Input.Impl.requires role impl
                  |> Fiber.sequential_iter ~f:(fun (dep : Input.dependency) ->
                    match dep.importance with
                    | Ensure -> process_dep expand_deps impl_var dep
                    | Prevent ->
                      (* Defer processing restricting deps until all essential
                         deps have been processed for the entire problem.
                         Restricting deps will be processed later without
                         recurring into their dependencies. *)
                      deferred := (impl_var, dep) :: !deferred;
                      Fiber.return ()))
            in
            clause
        and process_dep expand_deps user_var (dep : Input.dependency) : unit Fiber.t =
          (* Process a dependency of [user_var]:
             - find the candidate implementations to satisfy it
             - take just those that satisfy any restrictions in the dependency
             - ensure that we don't pick an incompatbile version if we select
               [user_var]
             - ensure that we do pick a compatible version if we select
               [user_var] (for "essential" dependencies only) *)
          let+ pass, fail =
            let+ candidates = lookup_impl expand_deps dep.drole in
            List.partition_map candidates.vars ~f:(fun { var; impl } ->
              if List.for_all ~f:(Input.meets_restriction impl) dep.restrictions
              then Left var
              else Right var)
          in
          match dep.importance with
          | Ensure ->
            Sat.implies
              sat
              ~reason:"essential dep"
              user_var
              pass (* Must choose a suitable candidate *)
          | Prevent ->
            (* If [user_var] is selected, don't select an incompatible version of
               the optional dependency. We don't need to do this explicitly in
               the [essential] case, because we must select a good version and we can't
               select two. *)
            (try
               let (_ : Sat.at_most_one_clause) = Sat.at_most_one (user_var :: fail) in
               ()
             with
             | Invalid_argument reason ->
               (* Explicitly conflicts with itself! *)
               Sat.at_least_one sat [ Sat.neg user_var ] ~reason)
        in
        let conflicts = ref [] in
        let* () =
          (* This recursively builds the whole problem up. *)
          let+ candidates =
            lookup_impl (`Expand_and_collect_conflicts conflicts) root_req
          in
          List.map candidates.vars ~f:(fun x -> x.var)
          |> Sat.at_least_one sat ~reason:"need root" (* Must get what we came for! *)
        in
        (* Now process any restricting deps. Due to the cache, only restricting
           deps that aren't also an essential dep will be expanded. The solver will
           not process any transitive dependencies here since the dependencies of
           restricting dependencies are irrelevant to solving the dependency
           problem. *)
        List.rev !conflicts
        |> Fiber.sequential_iter ~f:(fun (impl_var, dep) ->
          process_dep `No_expand impl_var dep)
        (* All impl_candidates have now been added, so snapshot the cache. *)
      in
      let impl_clauses = !impl_cache in
      Conflict_classes.seal conflict_classes;
      impl_clauses
    ;;

    (** [do_solve model req] finds an implementation matching the given
        requirements, plus any other implementations needed
        to satisfy its dependencies.

        @param closest_match
          adds a lowest-ranked (but valid) implementation ([Input.dummy_impl]) to
          every interface, so we can always select something. Useful for diagnostics.
          Note: always try without [closest_match] first, or it may miss a valid solution.
        @return None if the solve fails (only happens if [closest_match] is false). *)
    let do_solve context ~closest_match root_req =
      (* The basic plan is this:
         1. Scan the root interface and all dependencies recursively, building up a SAT problem.
         2. Solve the SAT problem. Whenever there are multiple options, try the most preferred one first.
         3. Create the selections XML from the results.

         All three involve recursively walking the tree in a similar way:
         1) we follow every dependency of every implementation (order not important)
         2) we follow every dependency of every selected implementation (better versions first)
         3) we follow every dependency of every selected implementation
      *)
      let sat = Sat.create () in
      let dummy_impl = if closest_match then Some Input.Dummy else None in
      let+ impl_clauses = build_problem context root_req sat ~dummy_impl in
      (* Run the solve *)
      let decider () =
        (* Walk the current solution, depth-first, looking for the first
           undecided interface. Then try the most preferred implementation of
           it that hasn't been ruled out. *)
        let seen = Table.create (module Input.Role) 100 in
        let rec find_undecided req =
          if Table.mem seen req
          then None (* Break cycles *)
          else (
            Table.set seen req true;
            match Input.Role.Map.find_exn impl_clauses req |> Candidates.state with
            | Unselected -> None
            | Undecided lit -> Some lit
            | Selected deps ->
              (* We've already selected a candidate for this component. Now
                 check its dependencies. *)
              List.find_map deps ~f:(fun (dep : Input.dependency) ->
                match dep.importance with
                | Ensure -> find_undecided dep.drole
                | Prevent ->
                  (* Restrictions don't express that we do or don't want the
                     dependency, so skip them here. If someone else needs this,
                     we'll handle it when we get to them.
                     If noone wants it, it will be set to unselected at the end. *)
                  None))
        in
        find_undecided root_req
      in
      match Sat.run_solver sat decider with
      | false -> None
      | true ->
        (* Build the results object *)
        Some (Input.Role.Map.filter_map impl_clauses ~f:Candidates.selected)
    ;;
  end

  module Diagnostics = struct
    let format_restrictions r =
      String.concat ~sep:", " (List.map ~f:Input.Restriction.to_string r)
    ;;

    module Note = struct
      (** An item of information to display for a component. *)
      type t =
        | UserRequested of Input.Restriction.t
        | Restricts of Input.Role.t * Input.Impl.t * Input.Restriction.t list
        | Feed_problem of string

      let pp = function
        | UserRequested r -> Pp.paragraphf "User requested %s" (format_restrictions [ r ])
        | Restricts (other_role, impl, r) ->
          Pp.hovbox
            ~indent:2
            (Input.Role.pp other_role
             ++ Pp.char ' '
             ++ Input.pp_version impl
             ++ Pp.text " requires "
             ++ Pp.paragraph (format_restrictions r))
        | Feed_problem msg -> Pp.text msg
      ;;
    end

    (** Represents a single interface in the example (failed) selections produced by the solver.
        It partitions the implementations into good and bad based (initially) on the split from the
        impl_provider. As we explore the example selections, we further filter the candidates. *)
    module Component = struct
      type rejection_reason =
        [ `Model_rejection of Context.rejection
        | `FailsRestriction of Input.Restriction.t
        | `DepFailsRestriction of Input.dependency * Input.Restriction.t
        | `ClassConflict of Input.Role.t * OpamPackage.Name.t
        | `ConflictsRole of Input.Role.t
        | `DiagnosticsFailure of User_message.Style.t Pp.t
        ]
      (* Why a particular implementation was rejected. This could be because the model rejected it,
         or because it conflicts with something else in the example (partial) solution. *)

      type reject = Input.impl * rejection_reason

      type t =
        { role : Input.Role.t
        ; diagnostics : User_message.Style.t Pp.t Lazy.t
        ; selected_impl : Input.impl option
        ; (* orig_good is all the implementations passed to the SAT solver (these are the
             ones with a compatible OS, CPU, etc). They are sorted most desirable first. *)
          orig_good : Input.impl list
        ; orig_bad : (Input.impl * Context.rejection) list
        ; mutable good : Input.impl list
        ; mutable bad : (Input.impl * rejection_reason) list
        ; mutable notes : Note.t list
        }

      (* Initialise a new component.
         @param candidates is the result from the impl_provider.
         @param selected_impl
           is the selected implementation, or [None] if we chose [dummy_impl].
         @param diagnostics can be used to produce diagnostics as a last resort. *)
      let create
        ~role
        (candidates, orig_bad, feed_problems)
        (diagnostics : _ Pp.t Lazy.t)
        (selected_impl : Input.impl option)
        =
        let notes = List.map ~f:(fun x -> Note.Feed_problem x) feed_problems in
        { role
        ; orig_good = candidates
        ; orig_bad
        ; good = candidates
        ; bad = List.map ~f:(fun (impl, reason) -> impl, `Model_rejection reason) orig_bad
        ; notes
        ; diagnostics
        ; selected_impl
        }
      ;;

      let note t note = t.notes <- note :: t.notes
      let notes t = List.rev t.notes

      (* Did rejecting [impl] make any difference?
         If [t] selected a better version anyway then we don't need to report this rejection. *)
      let affected_selection t impl =
        match t.selected_impl with
        | Some selected when Input.Impl.compare_version selected impl = Gt -> false
        | _ -> true
      ;;

      (* Call [get_problem impl] on each good impl. If a problem is returned, move [impl] to [bad_impls].
         If anything changes and [!note] is not None, report it and clear the pending note. *)
      let filter_impls_ref ~note:n t get_problem =
        let old_good = List.rev t.good in
        t.good <- [];
        List.iter old_good ~f:(fun impl ->
          match get_problem impl with
          | None -> t.good <- impl :: t.good
          | Some problem ->
            Option.iter !n ~f:(fun info ->
              if affected_selection t impl
              then (
                note t info;
                n := None));
            t.bad <- (impl, problem) :: t.bad)
      ;;

      let filter_impls ?note t get_problem =
        let note = ref note in
        filter_impls_ref ~note t get_problem
      ;;

      (* Remove from [good_impls] anything that fails to meet these restrictions.
         Add removed items to [bad_impls], along with the cause. *)
      let apply_restrictions ~note t restrictions =
        let note = ref (Some note) in
        List.iter restrictions ~f:(fun r ->
          filter_impls_ref ~note t (fun impl ->
            if Input.meets_restriction impl r then None else Some (`FailsRestriction r)))
      ;;

      let apply_user_restriction t r =
        note t (UserRequested r);
        (* User restrictions should be applied before reaching the solver, but just in case: *)
        filter_impls t (fun impl ->
          if Input.meets_restriction impl r then None else Some (`FailsRestriction r));
        (* Completely remove non-matching impls.
           The user will only want to see the version they asked for. *)
        let new_bad =
          List.filter t.bad ~f:(fun (impl, _) ->
            if Input.meets_restriction impl r then true else false)
        in
        if new_bad <> [] || t.good <> [] then t.bad <- new_bad
      ;;

      let reject_all t reason =
        t.bad <- List.map ~f:(fun impl -> impl, reason) t.good @ t.bad;
        t.good <- []
      ;;

      let selected_impl t = t.selected_impl

      (* When something conflicts with itself then our usual trick of selecting
         the main implementation and failing the dependency doesn't work, so
         special-case that here. *)
      let reject_self_conflicts t =
        filter_impls t (fun impl ->
          let deps = Input.Impl.requires t.role impl in
          List.find_map deps ~f:(fun (dep : Input.dependency) ->
            match Input.Role.compare dep.drole t.role with
            | Lt | Gt -> None
            | Eq ->
              (* It depends on itself. *)
              List.find_map dep.restrictions ~f:(fun r ->
                if Input.meets_restriction impl r
                then None
                else Some (`DepFailsRestriction (dep, r)))))
      ;;

      let finalise t =
        if t.selected_impl = None
        then (
          reject_self_conflicts t;
          reject_all t (`DiagnosticsFailure (Lazy.force t.diagnostics)))
      ;;

      let pp_reject ((_impl, reason) : reject) =
        match reason with
        | `Model_rejection r -> Context.pp_rejection r
        | `FailsRestriction r ->
          Pp.paragraphf
            "Incompatible with restriction: %s"
            (Input.Restriction.to_string r)
        | `DepFailsRestriction (dep, restriction) ->
          Pp.hovbox
            (Pp.text "Requires "
             ++ Input.Role.pp dep.drole
             ++ Pp.textf " %s" (format_restrictions [ restriction ]))
        | `ClassConflict (other_role, cl) ->
          Pp.hovbox
            (Pp.textf "In same conflict class (%s) as " (OpamPackage.Name.to_string cl)
             ++ Input.Role.pp other_role)
        | `ConflictsRole other_role ->
          Pp.hovbox (Pp.text "Conflicts with " ++ Input.Role.pp other_role)
        | `DiagnosticsFailure msg ->
          Pp.hovbox (Pp.text "Reason for rejection unknown: " ++ msg)
      ;;

      let show_rejections ~verbose rejected =
        let by_version (a, _) (b, _) = Input.Impl.compare_version b a in
        let rejected = List.sort ~compare:by_version rejected in
        let rec aux i = function
          | [] -> Pp.nop
          | _ when i = 5 && not verbose -> Pp.cut ++ Pp.text "..."
          | (impl, problem) :: xs ->
            Pp.cut
            ++ Pp.hovbox
                 ~indent:2
                 (Input.pp_impl_long impl ++ Pp.text ": " ++ pp_reject (impl, problem))
            ++ aux (i + 1) xs
        in
        aux 0 rejected
      ;;

      let rejects t =
        let summary =
          if t.orig_good = []
          then if t.orig_bad = [] then `No_candidates else `All_unusable
          else `Conflicts
        in
        t.bad, summary
      ;;

      let pp_candidates ~verbose t =
        if t.selected_impl = None
        then
          Pp.cut
          ++
          match rejects t with
          | _, `No_candidates -> Pp.paragraph "No known implementations at all"
          | bad, `All_unusable ->
            Pp.vbox
              ~indent:2
              (Pp.paragraph "No usable implementations:" ++ show_rejections ~verbose bad)
          | bad, `Conflicts ->
            Pp.vbox
              ~indent:2
              (Pp.paragraph "Rejected candidates:" ++ show_rejections ~verbose bad)
        else Pp.nop
      ;;

      let pp_notes t =
        match notes t with
        | [] -> Pp.nop
        | notes -> Pp.cut ++ Pp.concat_map ~sep:Pp.cut notes ~f:Note.pp
      ;;

      let pp_outcome t =
        match t.selected_impl with
        | Some sel -> Input.pp_impl_long sel
        | None -> Pp.text "(problem)"
      ;;

      (* Format a textual description of this component's report. *)
      let pp ~verbose t =
        Pp.vbox
          ~indent:2
          (Pp.hovbox (Input.pp_role t.role ++ Pp.text " -> " ++ pp_outcome t)
           ++ pp_notes t
           ++ pp_candidates ~verbose t)
      ;;
    end

    (* Did any dependency of [impl] prevent it being selected?
       This can only happen if a component conflicts with something more
       important than itself (otherwise, we'd select something in [impl]'s
       interface and complain about the dependency instead).

       e.g. A depends on B and C. B and C both depend on D. C1 conflicts with
       D1. The depth-first priority order means we give priority to {A1, B1,
       D1}. Then we can't choose C1 because we prefer to keep D1. *)
    let get_dependency_problem role (report : Component.t Input.Role.Map.t) impl =
      Input.Impl.requires role impl
      |> List.find_map ~f:(fun (dep : Input.dependency) ->
        match Input.Role.Map.find report dep.drole with
        | None -> None (* Not in the selections => can't be part of a conflict *)
        | Some required_component ->
          (match Component.selected_impl required_component with
           | None -> None (* Dummy selection can't cause a conflict *)
           | Some dep_impl ->
             List.find_map dep.restrictions ~f:(fun r ->
               if Input.meets_restriction dep_impl r
               then None
               else Some (`DepFailsRestriction (dep, r)))))
    ;;

    (** A selected component has [dep] as a dependency. Use this to explain why some implementations
        of the required interface were rejected. *)
    let examine_dep
      requiring_role
      requiring_impl
      (report : Component.t Input.Role.Map.t)
      (dep : Input.dependency)
      =
      match Input.Role.Map.find report dep.drole with
      | None -> ()
      | Some required_component ->
        if dep.restrictions <> []
        then
          (* Remove implementations incompatible with the other selections *)
          Component.apply_restrictions
            required_component
            dep.restrictions
            ~note:(Restricts (requiring_role, requiring_impl, dep.restrictions))
    ;;

    (* Find all restrictions that are in play and affect this interface *)
    let examine_selection report role component =
      match Component.selected_impl component with
      | Some our_impl ->
        (* For each dependency of our selected impl, explain why it rejected
           impls in the dependency's interface. *)
        Input.Impl.requires role our_impl
        |> List.iter ~f:(examine_dep role our_impl report)
      | None ->
        (* For each of our remaining unrejected impls, check whether a
           dependency prevented its selection. *)
        get_dependency_problem role report |> Component.filter_impls component
    ;;

    (* Check for user-supplied restrictions *)
    let examine_extra_restrictions report context =
      Input.Role.Map.iteri report ~f:(fun role component ->
        Input.Role.user_restrictions role context
        |> Option.iter ~f:(Component.apply_user_restriction component))
    ;;

    (** For each selected implementation with a conflict class, reject all candidates
        with the same class. *)
    let check_conflict_classes report =
      let classes =
        Input.Role.Map.foldi
          report
          ~init:OpamPackage.Name.Map.empty
          ~f:(fun role component acc ->
            match Component.selected_impl component with
            | None -> acc
            | Some impl ->
              Input.Impl.conflict_class impl
              |> List.fold_left ~init:acc ~f:(fun acc x ->
                OpamPackage.Name.Map.add x role acc))
      in
      Input.Role.Map.iteri report ~f:(fun role component ->
        Component.filter_impls component (fun impl ->
          Input.Impl.conflict_class impl
          |> List.find_map ~f:(fun cl ->
            match OpamPackage.Name.Map.find_opt cl classes with
            | Some other_role
              when not (Ordering.is_eq (Input.Role.compare role other_role)) ->
              Some (`ClassConflict (other_role, cl))
            | _ -> None)))
    ;;

    let of_result context impls =
      let explain role =
        match Input.Role.Map.find impls role with
        | Some (sel : Solver.selection) -> Solver.Sat.explain_reason sel.var
        | None -> Pp.text "Role not used!"
      in
      let+ report =
        let get_selected role (sel : Solver.selection) =
          let diagnostics = lazy (explain role) in
          let impl = if sel.impl = Input.Dummy then None else Some sel.impl in
          (* CR rgrinberg: Are we recomputing things here? *)
          let* impl_candidates = Input.implementations role context in
          let+ rejects, feed_problems = Input.Role.rejects role context in
          Component.create
            ~role
            (impl_candidates, rejects, feed_problems)
            diagnostics
            impl
        in
        Input.Role.Map.to_list impls
        |> Fiber.parallel_map ~f:(fun (k, v) ->
          let+ v = get_selected k v in
          k, v)
        |> Fiber.map ~f:Input.Role.Map.of_list_exn
      in
      examine_extra_restrictions report context;
      check_conflict_classes report;
      Input.Role.Map.iteri ~f:(examine_selection report) report;
      Input.Role.Map.iteri ~f:(fun _ c -> Component.finalise c) report;
      report
    ;;
  end

  let solve context pkgs =
    let req =
      match pkgs with
      | [ pkg ] -> Input.Real pkg
      | pkgs ->
        let impl : Input.Impl.t =
          let depends =
            List.map pkgs ~f:(fun name ->
              { Input.drole = Real name; importance = Ensure; restrictions = [] })
          in
          VirtualImpl (Input.Rank.bottom, depends)
        in
        Input.virtual_role [ impl ]
    in
    Solver.do_solve context ~closest_match:false req
    >>| function
    | Some sels -> Ok sels
    | None -> Error req
  ;;

  let pp_rolemap ~verbose reasons =
    let good, bad, unknown =
      Input.Role.Map.to_list reasons
      |> List.partition_three ~f:(fun (role, component) ->
        match Diagnostics.Component.selected_impl component with
        | Some impl when Diagnostics.Component.notes component = [] -> `Left impl
        | _ ->
          (match Diagnostics.Component.rejects component with
           | _, `No_candidates -> `Right role
           | _, _ -> `Middle component))
    in
    let pp_bad = Diagnostics.Component.pp ~verbose in
    let pp_unknown role = Pp.box (Input.Role.pp role) in
    match unknown with
    | [] ->
      Pp.paragraph "Selected candidates: "
      ++ Pp.hovbox (Pp.concat_map ~sep:Pp.space good ~f:Input.pp_impl)
      ++ Pp.cut
      ++ Pp.enumerate bad ~f:pp_bad
    | _ ->
      (* In case of unknown packages, no need to print the full diagnostic
         list, the problem is simpler. *)
      Pp.hovbox
        (Pp.text "The following packages couldn't be found: "
         ++ Pp.concat_map ~sep:Pp.space unknown ~f:pp_unknown)
  ;;

  let diagnostics_rolemap context req =
    Solver.do_solve context req ~closest_match:true
    >>| Option.value_exn
    >>= Diagnostics.of_result context
  ;;

  let diagnostics ?(verbose = false) context req =
    let+ diag = diagnostics_rolemap context req in
    Pp.paragraph "Couldn't solve the package dependency formula."
    ++ Pp.cut
    ++ Pp.vbox (pp_rolemap ~verbose diag)
  ;;

  let packages_of_result sels =
    Input.Role.Map.values sels
    |> List.filter_map ~f:(fun (sel : Solver.selection) -> Input.Impl.version sel.impl)
  ;;
end

let is_valid_global_variable_name = function
  | "root" -> false
  | _ -> true
;;

(* CR-rgrinberg: we need this validation in substitution actions as well *)
let is_valid_package_variable_name = function
  | "hash" | "build-id" | "misc" | "opam-version" | "depends" | "build" | "opamfile" ->
    false
  | _ -> true
;;

let invalid_variable_error ~loc variable =
  User_error.raise
    ~loc
    [ Pp.textf "Variable %S is not supported." (OpamVariable.to_string variable) ]
;;

let opam_variable_to_slang ~loc packages variable =
  let variable_string = OpamVariable.to_string variable in
  let convert_with_package_name package_name =
    if not (is_valid_package_variable_name variable_string)
    then invalid_variable_error ~loc variable;
    let pform =
      let name = Package_variable_name.of_string variable_string in
      let scope : Package_variable.Scope.t =
        match package_name with
        | None -> Self
        | Some p -> Package (Package_name.of_opam_package_name p)
      in
      Package_variable.to_pform { Package_variable.name; scope }
    in
    Slang.pform pform
  in
  match packages with
  | [] ->
    if not (is_valid_global_variable_name variable_string)
    then
      (* Note that there's no syntactic distinction between global variables
         and package variables in the current package. This check will prevent
         invalid global variable names from being used for package variables in the
         current package where the optional qualifier "_:" is omitted. *)
      invalid_variable_error ~loc variable;
    (match Pform.Var.of_opam_global_variable_name variable_string with
     | Some global_var -> Slang.pform (Pform.Var global_var)
     | None -> convert_with_package_name None)
  | [ package_name ] -> convert_with_package_name package_name
  | many ->
    Slang.blang
      (Blang.And
         (List.map many ~f:(fun package_name ->
            Blang.Expr (convert_with_package_name package_name))))
;;

let opam_fident_to_slang ~loc fident =
  let packages, variable, string_converter = OpamFilter.desugar_fident fident in
  let slang = opam_variable_to_slang ~loc packages variable in
  match string_converter with
  | None -> slang
  | Some (then_, else_) ->
    (* The "else" case is also used when evaluating the condition would expand
       an undefined variable. The catch_undefined_var operator is used to
       convert expressions that throw undefined variable exceptions into false.
    *)
    let condition =
      Blang.Expr (Slang.catch_undefined_var slang ~fallback:(Slang.bool false))
    in
    Slang.if_ condition ~then_:(Slang.text then_) ~else_:(Slang.text else_)
;;

let opam_raw_fident_to_slang ~loc raw_ident =
  OpamTypesBase.filter_ident_of_string raw_ident |> opam_fident_to_slang ~loc
;;

let opam_string_to_slang ~package ~loc opam_string =
  Re.Seq.split_full OpamFilter.string_interp_regex opam_string
  |> Seq.map ~f:(function
    | `Text text -> Slang.text text
    | `Delim group ->
      (match Re.Group.get group 0 with
       | "%%" -> Slang.text "%"
       | interp
         when String.is_prefix ~prefix:"%{" interp && String.is_suffix ~suffix:"}%" interp
         ->
         let ident = String.sub ~pos:2 ~len:(String.length interp - 4) interp in
         opam_raw_fident_to_slang ~loc ident
       | other ->
         User_error.raise
           ~loc
           [ Pp.textf
               "Encountered malformed variable interpolation while processing commands \
                for package %s."
               (OpamPackage.to_string package)
           ; Pp.text "The variable interpolation:"
           ; Pp.text other
           ]))
  |> List.of_seq
  |> Slang.concat
;;

(* Translate an Opam filter into Dune's "Slang" DSL. The main difference between
   the two languages is in their treatment of undefined package variables. In
   Opam filters, undefined variables take on the value <undefined> which
   is "falsey" in some contexts and propagates through boolean operators if
   their result could be affected by the <undefined> term. Slang doesn't have an
   <undefined> value but raises an exception when an undefined variable is
   expanded. There are two operators in Slang for handling exceptions:

   - "(has_undefined_var <arg>)" evaluates <arg>, discarding the result, and
     returns a boolean which is true iff evaluating <arg> failed due to an
     undefined variable
   - "(catch_undefined_var <value> <fallback>)" evaluates <value> and returns
     the result unless evaluation failed due to an undefined variable, in which
     case the result of <fallback> is returned

   These two Slang operators are used to emulate Opam's undefined value
   semantics.
*)
let filter_to_blang ~package ~loc filter =
  let filter_to_slang (filter : OpamTypes.filter) =
    match filter with
    | FString s -> opam_string_to_slang ~package ~loc s
    | FIdent fident -> opam_fident_to_slang ~loc fident
    | other ->
      Code_error.raise
        "The opam file parser should only allow identifiers and strings in places where \
         strings are expected"
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "full filter", Dyn.string (OpamFilter.to_string filter)
        ; "non-string filter", Dyn.string (OpamFilter.to_string other)
        ]
  in
  let rec filter_to_blang (filter : OpamTypes.filter) =
    match filter with
    | FBool true -> Blang.Ast.true_
    | FBool false -> Blang.Ast.false_
    | (FString _ | FIdent _) as slangable -> Blang.Expr (filter_to_slang slangable)
    | FOp (lhs, op, rhs) ->
      let op = Package_dependency.Constraint.Op.of_opam op in
      Blang.Compare (op, filter_to_slang lhs, filter_to_slang rhs)
    | FAnd (lhs, rhs) ->
      Blang.Expr
        (Slang.and_absorb_undefined_var [ filter_to_blang lhs; filter_to_blang rhs ])
    | FOr (lhs, rhs) ->
      Blang.Expr
        (Slang.or_absorb_undefined_var [ filter_to_blang lhs; filter_to_blang rhs ])
    | FNot f -> Blang.Not (filter_to_blang f)
    | FDefined f ->
      let blang = filter_to_blang f in
      Blang.Not (Blang.Expr (Slang.has_undefined_var (Slang.blang blang)))
    | FUndef _ ->
      Code_error.raise
        "Encountered undefined filter which should not be possible since no filter \
         reduction has taken place."
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "filter", Dyn.string (OpamFilter.to_string filter)
        ]
  in
  filter_to_blang filter
;;

let simplify_filter get_solver_var =
  OpamFilter.partial_eval (fun var ->
    match OpamVariable.Full.scope var with
    | Global ->
      let name = OpamVariable.Full.variable var |> Package_variable_name.of_opam in
      if Package_variable_name.equal name Package_variable_name.with_test
      then
        (* We don't generate lockfiles for local packages, and we don't include
           test dependencies for non-local packages, so "with-test" always
           evaluates to "false". *)
        Some (B false)
      else get_solver_var name |> Option.map ~f:Variable_value.to_opam_variable_contents
    | _ -> None)
;;

let partial_eval_filter = function
  | None -> `Filter None
  | Some f ->
    let env = Fun.const None in
    (match OpamFilter.eval_to_bool env f with
     | exception Failure _ -> `Filter (Some f)
     | b -> if b then `Filter None else `Skip)
;;

let opam_commands_to_actions
  get_solver_var
  loc
  package
  (commands : OpamTypes.command list)
  =
  List.filter_map commands ~f:(fun (args, filter) ->
    let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
    match partial_eval_filter filter with
    | `Skip -> None
    | `Filter filter ->
      let terms =
        List.filter_map args ~f:(fun ((simple_arg : OpamTypes.simple_arg), filter) ->
          let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
          match partial_eval_filter filter with
          | `Skip -> None
          | `Filter filter ->
            let slang =
              let slang =
                match simple_arg with
                | CString s -> opam_string_to_slang ~package ~loc s
                | CIdent ident -> opam_raw_fident_to_slang ~loc ident
              in
              Slang.simplify slang
            in
            Some
              (Slang.simplify
                 (match filter with
                  | None -> slang
                  | Some filter ->
                    let filter_blang =
                      filter_to_blang ~package ~loc filter |> Slang.simplify_blang
                    in
                    Slang.when_ filter_blang slang)))
      in
      if List.is_empty terms
      then None
      else (
        let action =
          let action = Action.Run terms in
          match filter with
          | None -> action
          | Some filter ->
            let condition =
              filter_to_blang ~package ~loc filter |> Slang.simplify_blang
            in
            Action.When (condition, action)
        in
        Some action))
;;

let opam_env_update_to_env_update (var, env_op, value_string, _) : _ Action.Env_update.t =
  { Action.Env_update.op = env_op
  ; var
  ; value = String_with_vars.make_text Loc.none value_string
  }
;;

let make_action = function
  | [] -> None
  | [ action ] -> Some action
  | actions -> Some (Action.Progn actions)
;;

(* Heuristic to determine whether a package is an ocaml compiler *)
let opam_file_is_compiler (opam_package : OpamFile.OPAM.t) =
  (* Identify compiler packages by using the fact that all compiler
     Packages declare conflicts with other compiler packages. note
     that relying on the "compiler" flag to identify compiler packages
     will not work, as compiler options packages (such as
     ocaml-option-flambda) also have this flag. *)
  let ocaml_core_compiler = OpamPackage.Name.of_string "ocaml-core-compiler" in
  List.mem opam_package.conflict_class ocaml_core_compiler ~equal:OpamPackage.Name.equal
;;

let resolve_depopts ~resolve depopts =
  let rec collect acc depopts =
    match (depopts : OpamTypes.filtered_formula) with
    | Or ((Atom (_, _) as dep), depopts) -> collect (dep :: acc) depopts
    | Atom (_, _) as dep -> dep :: acc
    | Empty -> acc
    | _ ->
      (* We rely on depopts always being a list of or'ed package names. Opam
         verifies this for us at parsing time. Packages defined in dune-project
         files have this restriction for depopts and regular deps *)
      Code_error.raise "invalid depopts" [ "depopts", Opam_dyn.filtered_formula depopts ]
  in
  OpamFormula.ors_to_list depopts
  |> List.concat_map ~f:(fun x ->
    collect [] x
    |> List.rev
    |> List.concat_map ~f:(fun depopt ->
      match resolve depopt with
      | Error _ -> []
      | Ok { Resolve_opam_formula.post = _; regular } ->
        (* CR-someday rgrinberg: think about post deps *)
        regular))
;;

let opam_package_to_lock_file_pkg
  solver_env
  stats_updater
  version_by_package_name
  opam_package
  ~pinned_package_names
  ~(candidates_cache : (Package_name.t, Context.candidates) Table.t)
  =
  let name = Package_name.of_opam_package_name (OpamPackage.name opam_package) in
  let version =
    OpamPackage.version opam_package |> Package_version.of_opam_package_version
  in
  let resolved_package =
    (Table.find_exn candidates_cache name).resolved
    |> OpamPackage.Version.Map.find (Package_version.to_opam_package_version version)
  in
  let opam_file = Resolved_package.opam_file resolved_package in
  let loc = Resolved_package.loc resolved_package in
  let extra_sources =
    OpamFile.OPAM.extra_sources opam_file
    |> List.map ~f:(fun (opam_basename, opam_url) ->
      ( Path.Local.of_string (OpamFilename.Base.to_string opam_basename)
      , let url = Loc.none, OpamFile.URL.url opam_url in
        let checksum =
          match OpamFile.URL.checksum opam_url with
          | [] -> None
          | checksum :: _ -> Some (Loc.none, Checksum.of_opam_hash checksum)
        in
        { Source.url; checksum } ))
  in
  let info =
    let url = OpamFile.OPAM.url opam_file in
    let source =
      Option.map url ~f:(fun (url : OpamFile.URL.t) ->
        let checksum =
          OpamFile.URL.checksum url
          |> List.hd_opt
          |> Option.map ~f:(fun hash -> Loc.none, Checksum.of_opam_hash hash)
        in
        let url = Loc.none, OpamFile.URL.url url in
        { Source.url; checksum })
    in
    let dev =
      Package_name.Set.mem pinned_package_names name
      ||
      match url with
      | None -> false
      | Some url -> List.is_empty (OpamFile.URL.checksum url)
    in
    { Lock_dir.Pkg_info.name; version; dev; source; extra_sources }
  in
  let depends =
    let resolve what =
      Resolve_opam_formula.filtered_formula_to_package_names
        ~with_test:false
        (add_self_to_filter_env opam_package (Solver_env.to_env solver_env))
        version_by_package_name
        what
    in
    let depends =
      match resolve opam_file.depends with
      | Ok { regular; _ } -> regular
      | Error (`Formula_could_not_be_satisfied hints) ->
        Code_error.raise
          "Dependencies of package can't be satisfied from packages in solution"
          [ "package", Dyn.string (opam_package |> OpamPackage.to_string)
          ; "hints", Dyn.list Resolve_opam_formula.Unsatisfied_formula_hint.to_dyn hints
          ]
    in
    let depopts =
      resolve_depopts ~resolve opam_file.depopts
      |> List.filter ~f:(fun package_name ->
        not (List.mem depends package_name ~equal:Package_name.equal))
    in
    depends @ depopts |> List.map ~f:(fun package_name -> Loc.none, package_name)
  in
  let build_env action =
    let env_update =
      OpamFile.OPAM.build_env opam_file |> List.map ~f:opam_env_update_to_env_update
    in
    match env_update with
    | [] -> action
    | env_update -> Action.Withenv (env_update, action)
  in
  let get_solver_var variable_name =
    Solver_stats.Updater.expand_variable stats_updater variable_name;
    Solver_env.get solver_env variable_name
  in
  let build_command =
    if Resolved_package.dune_build resolved_package
    then Some Lock_dir.Build_command.Dune
    else (
      let subst_step =
        OpamFile.OPAM.substs opam_file
        |> List.map ~f:(fun x ->
          let x = OpamFilename.Base.to_string x in
          let input = String_with_vars.make_text Loc.none (x ^ ".in") in
          let output = String_with_vars.make_text Loc.none x in
          Action.Substitute (input, output))
      in
      let patch_step =
        OpamFile.OPAM.patches opam_file
        |> List.map ~f:(fun (basename, filter) ->
          let action =
            Action.Patch
              (String_with_vars.make_text Loc.none (OpamFilename.Base.to_string basename))
          in
          match filter with
          | None -> action
          | Some filter ->
            Action.When
              ( filter_to_blang ~package:opam_package ~loc:Loc.none filter
                |> Slang.simplify_blang
              , action ))
      in
      let build_step =
        opam_commands_to_actions
          get_solver_var
          loc
          opam_package
          (OpamFile.OPAM.build opam_file)
      in
      List.concat [ subst_step; patch_step; build_step ]
      |> make_action
      |> Option.map ~f:build_env
      |> Option.map ~f:(fun action -> Lock_dir.Build_command.Action action))
  in
  let depexts =
    OpamFile.OPAM.depexts opam_file
    |> List.concat_map ~f:(fun (sys_pkgs, filter) ->
      let env = Solver_env.to_env solver_env in
      if OpamFilter.eval_to_bool ~default:false env filter
      then OpamSysPkg.Set.to_list_map OpamSysPkg.to_string sys_pkgs
      else [])
  in
  let install_command =
    OpamFile.OPAM.install opam_file
    |> opam_commands_to_actions get_solver_var loc opam_package
    |> make_action
    |> Option.map ~f:build_env
  in
  let exported_env =
    OpamFile.OPAM.env opam_file |> List.map ~f:opam_env_update_to_env_update
  in
  let kind = if opam_file_is_compiler opam_file then `Compiler else `Non_compiler in
  ( kind
  , { Lock_dir.Pkg.build_command; install_command; depends; depexts; info; exported_env }
  )
;;

let solve_package_list packages ~context =
  Fiber.collect_errors (fun () ->
    (* [Solver.solve] returns [Error] when it's unable to find a solution to
       the dependencies, but can also raise exceptions, for example if opam
       is unable to parse an opam file in the package repository. To prevent
       an unexpected opam exception from crashing dune, we catch all
       exceptions raised by the solver and report them as [User_error]s
       instead. *)
    Solver.solve context packages)
  >>| (function
         | Ok (Ok res) -> Ok res
         | Ok (Error e) -> Error (`Diagnostics e)
         | Error [] -> assert false
         | Error (exn :: _) ->
           (* CR-rgrinberg: this needs to be handled right *)
           Error (`Exn exn))
  >>= function
  | Ok packages -> Fiber.return @@ Ok (Solver.packages_of_result packages)
  | Error (`Diagnostics e) ->
    let+ diagnostics = Solver.diagnostics context e in
    Error (`Diagnostic_message diagnostics)
  | Error (`Exn exn) ->
    (match exn.exn with
     | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format ->
       (* CR-rgrinberg: needs to include locations *)
       User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format) ]
     | User_error.E _ -> reraise exn.exn
     | _ ->
       Code_error.raise
         "Unexpected exception raised while solving dependencies"
         [ "exception", Exn_with_backtrace.to_dyn exn ])
;;

module Solver_result = struct
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_name.Map.Multi.t
    ; pinned_packages : Package_name.Set.t
    ; num_expanded_packages : int
    }
end

let reject_unreachable_packages =
  let reachable deps_of_package ~roots =
    let seen = ref Package_name.Set.empty in
    let rec loop = function
      | [] -> ()
      | pkg :: rest ->
        if Package_name.Set.mem !seen pkg
        then loop rest
        else (
          seen := Package_name.Set.add !seen pkg;
          let deps =
            match Package_name.Map.find deps_of_package pkg with
            | None -> []
            | Some deps -> deps
          in
          loop (List.rev_append deps rest))
    in
    loop roots;
    !seen
  in
  fun solver_env ~dune_version ~local_packages ~pkgs_by_name ->
    let roots = Package_name.Map.keys local_packages in
    let pkgs_by_version =
      Package_name.Map.merge pkgs_by_name local_packages ~f:(fun name lhs rhs ->
        match lhs, rhs with
        | None, None -> assert false
        | Some _, Some _ ->
          Code_error.raise
            "package is both local and returned by solver"
            [ "name", Package_name.to_dyn name ]
        | Some (lock_dir_pkg : Lock_dir.Pkg.t), None -> Some lock_dir_pkg.info.version
        | None, Some _pkg ->
          let version = Package_version.of_string "dev" in
          Some version)
    in
    let pkgs_by_name =
      Package_name.Map.merge pkgs_by_name local_packages ~f:(fun name lhs rhs ->
        match lhs, rhs with
        | None, None -> assert false
        | Some _, Some _ ->
          Code_error.raise
            "package is both local and returned by solver"
            [ "name", Package_name.to_dyn name ]
        | Some (pkg : Lock_dir.Pkg.t), None -> Some (List.map pkg.depends ~f:snd)
        | None, Some (pkg : Local_package.For_solver.t) ->
          let formula = Dependency_formula.to_filtered_formula pkg.dependencies in
          (* Use `dev` because at this point we don't have any version *)
          let opam_package =
            OpamPackage.of_string (sprintf "%s.dev" (Package_name.to_string pkg.name))
          in
          let env = add_self_to_filter_env opam_package (Solver_env.to_env solver_env) in
          let resolved =
            Resolve_opam_formula.filtered_formula_to_package_names
              env
              ~with_test:true
              (Package_name.Map.set pkgs_by_version Dune_dep.name dune_version)
              formula
          in
          let deps =
            match resolved with
            | Ok { regular; post = _ (* discard post deps *) } ->
              (* remove Dune from the formula as we remove it from solutions *)
              List.filter regular ~f:(fun pkg ->
                not (Package_name.equal Dune_dep.name pkg))
            | Error _ ->
              Code_error.raise
                "can't find a valid solution for the dependencies"
                [ "name", Package_name.to_dyn pkg.name ]
          in
          let depopts =
            List.filter_map pkg.depopts ~f:(fun (d : Package_dependency.t) ->
              Option.some_if
                (Package_name.Map.mem local_packages d.name
                 || Package_name.Map.mem pkgs_by_name d.name)
                d.name)
          in
          Some (deps @ depopts))
    in
    reachable pkgs_by_name ~roots
;;

let solve_lock_dir
  solver_env
  version_preference
  repos
  ~local_packages
  ~pins:pinned_packages
  ~constraints
  =
  let pinned_package_names = Package_name.Set.of_keys pinned_packages in
  let stats_updater = Solver_stats.Updater.init () in
  let context =
    Context.create
      ~pinned_packages
      ~solver_env
      ~repos
      ~version_preference
      ~local_packages:
        (Package_name.Map.map local_packages ~f:Local_package.For_solver.to_opam_file)
      ~stats_updater
      ~constraints
  in
  Package_name.Map.to_list_map local_packages ~f:(fun name _ ->
    Package_name.to_opam_package_name name)
  |> solve_package_list ~context
  >>= function
  | Error _ as e -> Fiber.return e
  | Ok solution ->
    (* don't include local packages or dune in the lock dir *)
    let opam_packages_to_lock =
      let is_local_package = Package_name.Map.mem local_packages in
      List.filter solution ~f:(fun package ->
        let name = OpamPackage.name package |> Package_name.of_opam_package_name in
        (not (is_local_package name)) && not (Package_name.equal Dune_dep.name name))
    in
    let* candidates_cache = Fiber_cache.to_table context.candidates_cache in
    let ocaml, pkgs =
      let pkgs =
        let version_by_package_name =
          List.map solution ~f:(fun (package : OpamPackage.t) ->
            ( Package_name.of_opam_package_name (OpamPackage.name package)
            , Package_version.of_opam_package_version (OpamPackage.version package) ))
          |> Package_name.Map.of_list_exn
        in
        List.map opam_packages_to_lock ~f:(fun opam_package ->
          opam_package_to_lock_file_pkg
            solver_env
            stats_updater
            version_by_package_name
            opam_package
            ~pinned_package_names
            ~candidates_cache)
      in
      let ocaml =
        (* This doesn't allow the compiler to live in the source tree. Oh
           well, it's not possible now anyway. *)
        match
          List.filter_map pkgs ~f:(fun (kind, pkg) ->
            match kind with
            | `Compiler -> Some pkg.info.name
            | `Non_compiler -> None)
        with
        | [] -> None
        | [ x ] -> Some (Loc.none, x)
        | _ ->
          User_error.raise
            (* CR-rgrinberg: needs to include locations *)
            [ Pp.text "multiple compilers selected" ]
            ~hints:[ Pp.text "add a conflict" ]
      in
      let pkgs =
        Package_name.Map.of_list_map pkgs ~f:(fun (_kind, pkg) -> pkg.info.name, pkg)
      in
      ocaml, pkgs
    in
    let lock_dir =
      match pkgs with
      | Error (name, _pkg1, _pkg2) ->
        Code_error.raise
          "Solver selected multiple versions for the same package"
          [ "name", Package_name.to_dyn name ]
      | Ok pkgs_by_name ->
        let expanded_solver_variable_bindings =
          let stats = Solver_stats.Updater.snapshot stats_updater in
          Solver_stats.Expanded_variable_bindings.of_variable_set
            stats.expanded_variables
            solver_env
        in
        Package_name.Map.iter
          pkgs_by_name
          ~f:(fun { Lock_dir.Pkg.depends; info = { name; _ }; _ } ->
            List.iter depends ~f:(fun (loc, dep_name) ->
              if Package_name.Map.mem local_packages dep_name
              then
                User_error.raise
                  ~loc
                  [ Pp.textf
                      "Dune does not support packages outside the workspace depending on \
                       packages in the workspace. The package %S is not in the workspace \
                       but it depends on the package %S which is in the workspace."
                      (Package_name.to_string name)
                      (Package_name.to_string dep_name)
                  ]));
        let pkgs_by_name =
          let reachable =
            reject_unreachable_packages
              solver_env
              ~dune_version:(Package_version.of_opam_package_version context.dune_version)
              ~local_packages
              ~pkgs_by_name
          in
          Package_name.Map.filteri pkgs_by_name ~f:(fun name _ ->
            Package_name.Set.mem reachable name)
        in
        Lock_dir.create_latest_version
          pkgs_by_name
          ~local_packages:(Package_name.Map.values local_packages)
          ~ocaml
          ~repos:(Some repos)
          ~expanded_solver_variable_bindings
    in
    let+ files =
      let resolved_packages =
        List.map opam_packages_to_lock ~f:(fun opam_package ->
          let candidates =
            OpamPackage.name opam_package
            |> Package_name.of_opam_package_name
            |> Table.find_exn candidates_cache
          in
          OpamPackage.Version.Map.find
            (OpamPackage.version opam_package)
            candidates.resolved)
      in
      Resolved_package.get_opam_package_files resolved_packages
      >>| List.map2 resolved_packages ~f:(fun resolved_package entries ->
        let package_name =
          Resolved_package.package resolved_package
          |> OpamPackage.name
          |> Package_name.of_opam_package_name
        in
        package_name, entries)
      >>| List.filter ~f:(fun (_, entries) -> List.is_non_empty entries)
      >>| Package_name.Map.of_list_exn
    in
    Ok
      { Solver_result.lock_dir
      ; files
      ; pinned_packages = pinned_package_names
      ; num_expanded_packages = Context.count_expanded_packages context
      }
;;
