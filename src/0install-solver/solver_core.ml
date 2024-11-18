(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Select a compatible set of components to run a program. *)

module List = Stdune.List

module type CACHE_ENTRY = sig
  type t
  type value

  val compare : t -> t -> int
end

open Fiber.O

module Cache (CacheEntry : CACHE_ENTRY) : sig
  (** The cache is used in [build_problem], while the clauses are still being added. *)
  type t

  module M : Map.S with type key = CacheEntry.t

  (** Once the problem is built, an immutable snapshot is taken. *)
  type snapshot = CacheEntry.value M.t

  val create : unit -> t

  (** [lookup cache make key] will look up [key] in [cache].
      * If not found, create it with [value, process = make key], add [value] to the cache,
      * and then call [process ()] on it.
      * [make] must not be recursive (since the key hasn't been added yet),
      * but [process] can be. In other words, [make] does whatever setup *must*
      * be done before anyone can use this cache entry, while [process] does
      * setup that can be done afterwards. *)
  val lookup
    :  t
    -> (CacheEntry.t -> (CacheEntry.value * (unit -> unit Fiber.t)) Fiber.t)
    -> CacheEntry.t
    -> CacheEntry.value Fiber.t

  val snapshot : t -> snapshot
  val get : CacheEntry.t -> snapshot -> CacheEntry.value option
  val get_exn : CacheEntry.t -> snapshot -> CacheEntry.value
  val filter_map : (CacheEntry.t -> 'a -> 'b option) -> 'a M.t -> 'b M.t
end = struct
  module M = Map.Make (CacheEntry)

  type snapshot = CacheEntry.value M.t
  type t = snapshot ref

  let create () = ref M.empty

  let lookup table make key =
    match M.find_opt key !table with
    | Some x -> Fiber.return x
    | None ->
      let* value, process = make key in
      table := M.add key value !table;
      let+ () = process () in
      value
  ;;

  let snapshot table = !table
  let get = M.find_opt
  let get_exn = M.find

  let filter_map f m =
    M.merge
      (fun key ao _bo ->
        match ao with
        | Some x -> f key x
        | None -> assert false)
      m
      M.empty
  ;;
end

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

  class type candidates = object
    method get_clause : S.at_most_one_clause option
    method get_vars : S.lit list
    method get_state : decision_state
  end

  class impl_candidates
    role
    (clause : S.at_most_one_clause option)
    (vars : (S.lit * Model.impl) list)
    dummy_impl =
    let is_dummy =
      match dummy_impl with
      | None -> fun _ -> false
      | Some dummy_impl -> ( == ) dummy_impl
    in
    object (_ : #candidates)
      method get_clause = clause

      (** Get all variables, except dummy_impl (if present) *)
      method get_real_vars =
        vars
        |> List.filter_map ~f:(fun (var, impl) ->
          if is_dummy impl then None else Some var)

      method get_vars = List.map ~f:(fun (var, _impl) -> var) vars

      method get_selected =
        match clause with
        | None -> None (* There were never any candidates *)
        | Some clause ->
          (match S.get_selected clause with
           | None -> None
           | Some lit ->
             (match S.get_user_data_for_lit lit with
              | SolverData.ImplElem impl -> Some (lit, impl)
              | _ -> assert false))

      method get_state =
        match clause with
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
             Selected (Model.requires role impl)
           | None ->
             (match S.get_best_undecided clause with
              | Some lit -> Undecided lit
              | None -> Unselected (* No remaining candidates, and none was chosen. *)))

      (** Apply [test impl] to each implementation, partitioning the vars into two lists.
          Only defined for [impl_candidates]. *)
      method partition test =
        List.partition_map
          ~f:(fun (var, impl) -> if test impl then Stdune.Either.Left var else Right var)
          vars
    end

  module RoleEntry = struct
    include Model.Role

    type value = impl_candidates
  end

  module ImplCache = Cache (RoleEntry)
  module RoleMap = ImplCache.M

  type diagnostics = S.lit

  let explain = S.explain_reason

  type selection =
    { impl : Model.impl (** The implementation chosen to fill the role *)
    ; diagnostics : diagnostics (** Extra information useful for diagnostics *)
    }

  (* Make each interface conflict with its replacement (if any).
   * We do this at the end because if we didn't use the replacement feed, there's no need to conflict
   * (avoids getting it added to feeds_used). *)
  let add_replaced_by_conflicts sat impl_clauses =
    List.iter ~f:(fun (clause, replacement) ->
      ImplCache.get replacement impl_clauses
      |> Option.iter (fun replacement_candidates ->
        (* Our replacement was also added to [sat], so conflict with it. *)
        let our_vars = clause#get_real_vars in
        let replacements = replacement_candidates#get_real_vars in
        if our_vars <> [] && replacements <> []
        then
          (* Must select one implementation out of all candidates from both interfaces.
             Dummy implementations don't conflict, though. *)
          S.at_most_one sat (our_vars @ replacements) |> ignore))
  ;;

  module Conflict_classes = struct
    module Map = Map.Make (struct
        type t = Model.conflict_class

        let compare = compare
      end)

    type t =
      { sat : S.t
      ; mutable groups : S.lit list ref Map.t
      }

    let create sat = { sat; groups = Map.empty }

    let var t name =
      match Map.find_opt name t.groups with
      | Some v -> v
      | None ->
        let v = ref [] in
        t.groups <- Map.add name v t.groups;
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
      t.groups
      |> Map.iter
         @@ fun _name impls ->
         let impls = !impls in
         if List.length impls > 1 then S.at_most_one t.sat impls |> ignore
    ;;
  end

  (* Process a dependency of [user_var]:
     - find the candidate implementations to satisfy it
     - take just those that satisfy any restrictions in the dependency
     - ensure that we don't pick an incompatbile version if we select [user_var]
     - ensure that we do pick a compatible version if we select [user_var] (for "essential" dependencies only) *)
  let process_dep sat lookup_impl user_var dep : unit Fiber.t =
    let { Model.dep_role; dep_importance } = Model.dep_info dep in
    let dep_restrictions = Model.restrictions dep in
    (* Restrictions on the candidates *)
    let meets_restrictions impl =
      List.for_all ~f:(Model.meets_restriction impl) dep_restrictions
    in
    let+ candidates = lookup_impl dep_role in
    let pass, fail = candidates#partition meets_restrictions in
    if dep_importance = `Essential
    then
      S.implies
        sat
        ~reason:"essential dep"
        user_var
        pass (* Must choose a suitable candidate *)
    else (
      (* If [user_var] is selected, don't select an incompatible version of the optional dependency.
         We don't need to do this explicitly in the [essential] case, because we must select a good
         version and we can't select two. *)
      try S.at_most_one sat (user_var :: fail) |> ignore with
      | Invalid_argument reason ->
        (* Explicitly conflicts with itself! *)
        S.at_least_one sat [ S.neg user_var ] ~reason)
  ;;

  (* Add the implementations of an interface to the ImplCache (called the first time we visit it). *)
  let make_impl_clause sat ~dummy_impl replacements role =
    let+ { Model.replacement; impls } = Model.implementations role in
    (* Insert dummy_impl (last) if we're trying to diagnose a problem. *)
    let impls =
      match dummy_impl with
      | None -> impls
      | Some dummy_impl -> impls @ [ dummy_impl ]
    in
    let impls =
      List.map impls ~f:(fun impl ->
        let var = S.add_variable sat (SolverData.ImplElem impl) in
        var, impl)
    in
    let impl_clause =
      if impls <> [] then Some (S.at_most_one sat (List.map ~f:fst impls)) else None
    in
    let clause = new impl_candidates role impl_clause impls dummy_impl in
    (* If we have a <replaced-by>, remember to add a conflict with our replacement *)
    replacement
    |> Option.iter (fun replacement ->
      replacements := (clause, replacement) :: !replacements);
    clause, impls
  ;;

  (** Starting from [root_req], explore all the feeds and implementations we might need, adding
      * all of them to [sat_problem]. *)
  let build_problem root_req sat ~dummy_impl =
    (* For each (iface, source) we have a list of implementations. *)
    let impl_cache = ImplCache.create () in
    let conflict_classes = Conflict_classes.create sat in
    (* Handle <replaced-by> conflicts after building the problem. *)
    let replacements = ref [] in
    let rec add_impls_to_cache role =
      let+ clause, impls = make_impl_clause sat ~dummy_impl replacements role in
      ( clause
      , fun () ->
          impls
          |> Fiber.sequential_iter ~f:(fun (impl_var, impl) ->
            Conflict_classes.process conflict_classes impl_var impl;
            let deps = Model.requires role impl in
            process_deps impl_var deps) )
    and lookup_impl key = ImplCache.lookup impl_cache add_impls_to_cache key
    and process_deps user_var : _ -> unit Fiber.t =
      Fiber.sequential_iter ~f:(process_dep sat lookup_impl user_var)
    in
    let+ () =
      (* This recursively builds the whole problem up. *)
      (let+ impl = lookup_impl root_req in
       impl#get_vars)
      >>| S.at_least_one sat ~reason:"need root" (* Must get what we came for! *)
    in
    (* All impl_candidates have now been added, so snapshot the cache. *)
    let impl_clauses = ImplCache.snapshot impl_cache in
    add_replaced_by_conflicts sat impl_clauses !replacements;
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
      ; dep_importance : [ `Essential | `Recommended | `Restricts ]
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
    let lookup role = (ImplCache.get_exn role impl_clauses :> candidates) in
    (* Run the solve *)
    let decider () =
      (* Walk the current solution, depth-first, looking for the first undecided interface.
         Then try the most preferred implementation of it that hasn't been ruled out. *)
      let seen = Hashtbl.create 100 in
      let rec find_undecided req =
        if Hashtbl.mem seen req
        then None (* Break cycles *)
        else (
          Hashtbl.add seen req true;
          let candidates = lookup req in
          match candidates#get_state with
          | Unselected -> None
          | Undecided lit -> Some lit
          | Selected deps ->
            (* We've already selected a candidate for this component. Now check its dependencies. *)
            let check_dep dep =
              let { Model.dep_role; dep_importance } = Model.dep_info dep in
              if dep_importance = `Restricts
              then
                (* Restrictions don't express that we do or don't want the
                   dependency, so skip them here. If someone else needs this,
                   we'll handle it when we get to them.
                   If noone wants it, it will be set to unselected at the end. *)
                None
              else find_undecided dep_role
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
          candidates#get_selected
          |> Option.map (fun (lit, impl) -> { impl; diagnostics = lit }))
      in
      Some { Output.root_req; selections }
  ;;
end
