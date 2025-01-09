(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Select a compatible set of components to run a program. *)

open Stdune
open Fiber.O

module Make (Model : S.SOLVER_INPUT) = struct
  (** We attach this data to each SAT variable. *)
  module SolverData = struct
    type t =
      (* If the SAT variable is True then we selected this... *)
      | ImplElem of Model.impl
      | Role of Model.Role.t

    let pp = function
      | ImplElem impl -> Model.pp_impl impl
      | Role role -> Model.Role.pp role
    ;;
  end

  module S = Sat.Make (SolverData)

  type decision_state =
    (* The next candidate to try *)
    | Undecided of S.lit
    (* The dependencies to check next *)
    | Selected of Model.dependency list
    | Unselected

  module Candidates = struct
    type t =
      { role : Model.Role.t
      ; clause : S.at_most_one_clause option
      ; vars : (S.lit * Model.impl) list
      ; dummy_impl : Model.impl option
      }

    let is_dummy t impl =
      match t.dummy_impl with
      | None -> false
      | Some dummy_impl -> dummy_impl == impl
    ;;

    let create role clause vars dummy_impl = { role; clause; vars; dummy_impl }
    let vars t = List.map ~f:fst t.vars

    let selected t =
      let open Stdune.Option.O in
      let* lit = t.clause >>= S.get_selected in
      match S.get_user_data_for_lit lit with
      | SolverData.ImplElem impl -> Some (lit, impl)
      | _ -> assert false
    ;;

    let state t =
      match t.clause with
      | None -> Unselected (* There were never any candidates *)
      | Some clause ->
        (match S.get_selected clause with
         | Some lit ->
           (* We've already chosen which <implementation> to use. Follow dependencies. *)
           let impl =
             match S.get_user_data_for_lit lit with
             | SolverData.ImplElem impl -> impl
             | _ -> assert false
           in
           Selected (Model.requires t.role impl)
         | None ->
           (match S.get_best_undecided clause with
            | Some lit -> Undecided lit
            | None -> Unselected (* No remaining candidates, and none was chosen. *)))
    ;;

    (* Apply [test impl] to each implementation, partitioning the vars into two
       lists. Only defined for [impl_candidates]. *)
    let partition t ~f:test =
      List.partition_map t.vars ~f:(fun (var, impl) ->
        if test impl then Stdune.Either.Left var else Right var)
    ;;
  end

  module ImplCache = Cache.Make (Model.Role)
  module RoleMap = ImplCache.M

  type diagnostics = S.lit

  let explain = S.explain_reason

  type selection =
    { impl : Model.impl (** The implementation chosen to fill the role *)
    ; diagnostics : diagnostics (** Extra information useful for diagnostics *)
    }

  module Conflict_classes = struct
    module Map = Map.Make (struct
        type t = Model.conflict_class

        let compare (x : t) (y : t) = String.compare (x :> string) (y :> string)
        let to_dyn (x : t) = Dyn.string (x :> string)
      end)

    type t =
      { sat : S.t
      ; mutable groups : S.lit list ref Map.t
      }

    let create sat = { sat; groups = Map.empty }

    let var t name =
      match Map.find t.groups name with
      | Some v -> v
      | None ->
        let v = ref [] in
        t.groups <- Map.set t.groups name v;
        v
    ;;

    (* Add [impl] to its conflict groups, if any. *)
    let process t impl_var impl =
      Model.conflict_class impl
      |> List.iter ~f:(fun name ->
        let impls = var t name in
        impls := impl_var :: !impls)
    ;;

    (* Call this at the end to add the final clause with all discovered groups.
       [t] must not be used after this. *)
    let seal t =
      Map.iter t.groups ~f:(fun impls ->
        let impls = !impls in
        if List.length impls > 1
        then (
          let (_ : S.at_most_one_clause) = S.at_most_one t.sat impls in
          ()))
    ;;
  end

  (* Add the implementations of an interface to the ImplCache (called the first time we visit it). *)
  let make_impl_clause sat ~dummy_impl role =
    let+ { impls } = Model.implementations role in
    (* Insert dummy_impl (last) if we're trying to diagnose a problem. *)
    let impls =
      (match dummy_impl with
       | None -> impls
       | Some dummy_impl -> impls @ [ dummy_impl ])
      |> List.map ~f:(fun impl ->
        let var = S.add_variable sat (SolverData.ImplElem impl) in
        var, impl)
    in
    let clause =
      let impl_clause =
        match impls with
        | [] -> None
        | _ :: _ -> Some (S.at_most_one sat (List.map ~f:fst impls))
      in
      Candidates.create role impl_clause impls dummy_impl
    in
    clause, impls
  ;;

  (** Starting from [root_req], explore all the feeds and implementations we
      might need, adding all of them to [sat_problem]. *)
  let build_problem root_req sat ~dummy_impl =
    (* For each (iface, source) we have a list of implementations. *)
    let impl_cache = ImplCache.create () in
    let conflict_classes = Conflict_classes.create sat in
    let deferred = ref [] in
    let expand_deps = ref true in
    let+ () =
      let rec lookup_impl =
        let add_impls_to_cache role =
          let+ clause, impls = make_impl_clause sat ~dummy_impl role in
          ( clause
          , fun () ->
              Fiber.sequential_iter impls ~f:(fun (impl_var, impl) ->
                Conflict_classes.process conflict_classes impl_var impl;
                if !expand_deps
                then
                  Model.requires role impl
                  |> Fiber.sequential_iter ~f:(fun dep ->
                    let { Model.dep_importance; _ } = Model.dep_info dep in
                    match dep_importance with
                    | `Essential -> process_dep impl_var dep
                    | `Restricts ->
                      (* Defer processing restricting deps until all essential deps have
                         been processed for the entire problem. Restricting deps will be
                         processed later without recurring into their dependencies. *)
                      deferred := (impl_var, dep) :: !deferred;
                      Fiber.return ())
                else Fiber.return ()) )
        in
        fun key -> ImplCache.lookup impl_cache add_impls_to_cache key
      and process_dep user_var dep : unit Fiber.t =
        (* Process a dependency of [user_var]:
           - find the candidate implementations to satisfy it
           - take just those that satisfy any restrictions in the dependency
           - ensure that we don't pick an incompatbile version if we select
             [user_var]
           - ensure that we do pick a compatible version if we select
             [user_var] (for "essential" dependencies only) *)
        let { Model.dep_role; dep_importance } = Model.dep_info dep in
        let+ pass, fail =
          let meets_restrictions =
            (* Restrictions on the candidates *)
            let dep_restrictions = Model.restrictions dep in
            fun impl -> List.for_all ~f:(Model.meets_restriction impl) dep_restrictions
          in
          lookup_impl dep_role >>| Candidates.partition ~f:meets_restrictions
        in
        match dep_importance with
        | `Essential ->
          S.implies
            sat
            ~reason:"essential dep"
            user_var
            pass (* Must choose a suitable candidate *)
        | `Restricts ->
          (* If [user_var] is selected, don't select an incompatible version of
             the optional dependency. We don't need to do this explicitly in
             the [essential] case, because we must select a good version and we can't
             select two. *)
          (try S.at_most_one sat (user_var :: fail) |> ignore with
           | Invalid_argument reason ->
             (* Explicitly conflicts with itself! *)
             S.at_least_one sat [ S.neg user_var ] ~reason)
      in
      let* () =
        (* This recursively builds the whole problem up. *)
        lookup_impl root_req
        >>| Candidates.vars
        >>| S.at_least_one sat ~reason:"need root" (* Must get what we came for! *)
      in
      (* Now process any restricting deps. Due to the cache, only restricting
         deps that aren't also an essential dep will be expanded. The solver will
         not process any transitive dependencies here since the dependencies of
         restricting dependencies are irrelevant to solving the dependency
         problem. *)
      expand_deps := false;
      Fiber.sequential_iter !deferred ~f:(fun (impl_var, dep) -> process_dep impl_var dep)
      (* All impl_candidates have now been added, so snapshot the cache. *)
    in
    let impl_clauses = ImplCache.snapshot impl_cache in
    Conflict_classes.seal conflict_classes;
    impl_clauses
  ;;

  module Output = struct
    module Input = Model
    module Role = Input.Role
    module RoleMap = RoleMap

    type impl = selection
    type dependency = Model.dependency

    type dep_info = Model.dep_info =
      { dep_role : Role.t
      ; dep_importance : [ `Essential | `Restricts ]
      }

    type requirements = Role.t

    let dep_info = Model.dep_info
    let requires role impl = Model.requires role impl.impl

    type t =
      { root_req : requirements
      ; selections : selection RoleMap.t
      }

    let to_map t = t.selections
    let requirements t = t.root_req

    let explain t role =
      match RoleMap.find_opt role t.selections with
      | Some sel -> explain sel.diagnostics
      | None -> Pp.text "Role not used!"
    ;;

    let get_selected role t =
      match RoleMap.find_opt role t.selections with
      | Some selection when selection.impl == Model.dummy_impl -> None
      | x -> x
    ;;

    let unwrap sel = sel.impl
  end

  let do_solve ~closest_match root_req =
    (* The basic plan is this:
       1. Scan the root interface and all dependencies recursively, building up a SAT problem.
       2. Solve the SAT problem. Whenever there are multiple options, try the most preferred one first.
       3. Create the selections XML from the results.

       All three involve recursively walking the tree in a similar way:
       1) we follow every dependency of every implementation (order not important)
       2) we follow every dependency of every selected implementation (better versions first)
       3) we follow every dependency of every selected implementation
    *)
    let sat = S.create () in
    let dummy_impl = if closest_match then Some Model.dummy_impl else None in
    let+ impl_clauses = build_problem root_req sat ~dummy_impl in
    let lookup role = ImplCache.get_exn role impl_clauses in
    (* Run the solve *)
    let decider () =
      (* Walk the current solution, depth-first, looking for the first undecided interface.
         Then try the most preferred implementation of it that hasn't been ruled out. *)
      let seen =
        let module Requirements = struct
          type t = Output.requirements

          let equal x y = Int.equal 0 (Output.Role.compare x y)
          let hash = Poly.hash
          let to_dyn = Dyn.opaque
        end
        in
        Table.create (module Requirements) 100
      in
      let rec find_undecided req =
        if Table.mem seen req
        then None (* Break cycles *)
        else (
          Table.set seen req true;
          let candidates = lookup req in
          match Candidates.state candidates with
          | Unselected -> None
          | Undecided lit -> Some lit
          | Selected deps ->
            (* We've already selected a candidate for this component. Now check its dependencies. *)
            let check_dep dep =
              let { Model.dep_role; dep_importance } = Model.dep_info dep in
              match dep_importance with
              | `Restricts ->
                (* Restrictions don't express that we do or don't want the
                   dependency, so skip them here. If someone else needs this,
                   we'll handle it when we get to them.
                   If noone wants it, it will be set to unselected at the end. *)
                None
              | `Essential -> find_undecided dep_role
            in
            List.find_map ~f:check_dep deps)
      in
      find_undecided root_req
    in
    match S.run_solver sat decider with
    | None -> None
    | Some _solution ->
      (* Build the results object *)
      let selections =
        impl_clauses
        |> ImplCache.filter_map (fun _role candidates ->
          Candidates.selected candidates
          |> Option.map ~f:(fun (lit, impl) -> { impl; diagnostics = lit }))
      in
      Some { Output.root_req; selections }
  ;;
end
