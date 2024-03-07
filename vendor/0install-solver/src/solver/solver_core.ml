(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Select a compatible set of components to run a program. *)

module List = struct
  include List

  let rec find_map f = function
    | [] -> None
    | (x::xs) -> match f x with
      | Some _ as result -> result
      | None -> find_map f xs
end

type ('a, 'b) partition_result =
  | Left of 'a
  | Right of 'b

let partition fn lst =
  let pass = ref [] in
  let fail = ref [] in
  ListLabels.iter lst ~f:(fun item ->
    match fn item with
    | Left x -> pass := x :: !pass
    | Right x -> fail := x :: !fail
  );
  (List.rev !pass, List.rev !fail)

module type CACHE_ENTRY = sig
  type t
  type value
  val compare : t -> t -> int
end

module Cache (Monad : S.Monad)(CacheEntry : CACHE_ENTRY) : sig
  (** The cache is used in [build_problem], while the clauses are still being added. *)
  type t

  module M : Map.S with
   type key = CacheEntry.t

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
  val lookup : t -> (CacheEntry.t -> (CacheEntry.value * (unit -> unit Monad.t)) Monad.t) -> CacheEntry.t -> CacheEntry.value Monad.t

  val snapshot : t -> snapshot
  val get : CacheEntry.t -> snapshot -> CacheEntry.value option
  val get_exn : CacheEntry.t -> snapshot -> CacheEntry.value

  val filter_map : (CacheEntry.t -> 'a -> 'b option) -> 'a M.t -> 'b M.t
end = struct
  module M = Map.Make(CacheEntry)

  type snapshot = CacheEntry.value M.t
  type t = snapshot ref

  let create () = ref M.empty

  let lookup table make key =
    let open Monad.O in
    match M.find_opt key !table with
    | Some x -> Monad.return x
    | None ->
      let* value, process = make key in
      table := M.add key value !table;
      let+ () = process () in
      value

  let snapshot table = !table

  let get = M.find_opt
  let get_exn = M.find

  let filter_map f m =
    M.merge (fun key ao _bo ->
      match ao with
      | Some x -> f key x
      | None -> assert false
    ) m M.empty
end

module Make (Monad : S.Monad) (Model : S.SOLVER_INPUT with type 'a monad = 'a Monad.t) = struct
  open Monad.O
  type 'a monad = 'a Monad.t

  (** We attach this data to each SAT variable. *)
  module SolverData =
    struct
      type t = (* If the SAT variable is True then we selected this... *)
        | ImplElem of Model.impl
        | CommandElem of Model.command
        | MachineGroup of string
        | Role of Model.Role.t
      let pp f = function
        | ImplElem impl -> Model.pp_impl f impl
        | CommandElem command -> Model.pp_command f command
        | MachineGroup name -> Format.pp_print_string f name
        | Role role -> Model.Role.pp f role
    end

  module S = Sat.Make(SolverData)

  type decision_state =
    (* The next candidate to try *)
    | Undecided of S.lit
    (* The dependencies to check next *)
    | Selected of (Model.dependency list * Model.command_name list)
    | Unselected

  class type candidates =
    object
      method get_clause : S.at_most_one_clause option
      method get_vars : S.lit list
      method get_state : decision_state
    end

  class impl_candidates role (clause : S.at_most_one_clause option) (vars : (S.lit * Model.impl) list) dummy_impl =
    let is_dummy =
      match dummy_impl with
      | None -> fun _ -> false
      | Some dummy_impl -> (==) dummy_impl in

    object (_ : #candidates)
      method get_clause = clause

      (** Get just those implementations that have a command with this name. *)
      method get_commands name =
        let match_command (impl_var, impl) =
          match Model.get_command impl name with
          | Some command -> Some (impl_var, command)
          | None -> None in
        vars |> List.filter_map match_command

      (** Get all variables, except dummy_impl (if present) *)
      method get_real_vars =
        vars |> List.filter_map (fun (var, impl) ->
          if is_dummy impl then None
          else Some var
        )

      method get_vars =
        List.map (fun (var, _impl) -> var) vars

      method get_selected =
        match clause with
        | None -> None      (* There were never any candidates *)
        | Some clause ->
            match S.get_selected clause with
            | None -> None
            | Some lit ->
                match S.get_user_data_for_lit lit with
                  | SolverData.ImplElem impl -> Some (lit, impl)
                  | _ -> assert false

      method get_state =
        match clause with
        | None -> Unselected      (* There were never any candidates *)
        | Some clause ->
            match S.get_selected clause with
            | Some lit ->
                (* We've already chosen which <implementation> to use. Follow dependencies. *)
                let impl = match S.get_user_data_for_lit lit with
                  | SolverData.ImplElem impl -> impl
                  | _ -> assert false in
                Selected (Model.requires role impl)
            | None ->
                match S.get_best_undecided clause with
                | Some lit -> Undecided lit
                | None -> Unselected        (* No remaining candidates, and none was chosen. *)

        (** Apply [test impl] to each implementation, partitioning the vars into two lists.
            Only defined for [impl_candidates]. *)
        method partition test = partition (fun (var, impl) -> if test impl then Left var else Right var) vars
    end

  (** Holds all the commands with a given name within an interface. *)
  class command_candidates role (clause : S.at_most_one_clause option) (vars : (S.lit * Model.command) list) =
    object (_ : #candidates)
      method get_clause = clause

      method get_vars =
        List.map (fun (var, _command) -> var) vars

      method get_state =
        match clause with
        | None -> Unselected      (* There were never any candidates *)
        | Some clause ->
            match S.get_selected clause with
            | Some lit ->
                (* We've already chosen which <command> to use. Follow dependencies. *)
                let command = match S.get_user_data_for_lit lit with
                  | SolverData.CommandElem command -> command
                  | _ -> assert false in
                Selected (Model.command_requires role command)
            | None ->
                match S.get_best_undecided clause with
                | Some lit -> Undecided lit
                | None -> Unselected        (* No remaining candidates, and none was chosen. *)
    end

  module CommandRoleEntry =
    struct
      type t = (Model.command_name * Model.Role.t)
      type value = command_candidates
      let compare ((an, ar):t) ((bn, br):t) =
        match String.compare (an :> string) (bn :> string) with
        | 0 -> Model.Role.compare ar br
        | r -> r
    end

  module RoleEntry =
    struct
      include Model.Role
      type value = impl_candidates
    end

  module ImplCache = Cache(Monad)(RoleEntry)
  module CommandCache = Cache(Monad)(CommandRoleEntry)

  module RoleMap = ImplCache.M

  type diagnostics = S.lit
  let explain = S.explain_reason

  type selection = {
    impl : Model.impl;                  (** The implementation chosen to fill the role *)
    commands : Model.command_name list; (** The commands required *)
    diagnostics : diagnostics;          (** Extra information useful for diagnostics *)
  }

  (* Make each interface conflict with its replacement (if any).
   * We do this at the end because if we didn't use the replacement feed, there's no need to conflict
   * (avoids getting it added to feeds_used). *)
  let add_replaced_by_conflicts sat impl_clauses =
    List.iter (fun (clause, replacement) ->
      ImplCache.get replacement impl_clauses
      |> Option.iter (fun replacement_candidates ->
        (* Our replacement was also added to [sat], so conflict with it. *)
        let our_vars = clause#get_real_vars in
        let replacements = replacement_candidates#get_real_vars in
        if (our_vars <> [] && replacements <> []) then (
          (* Must select one implementation out of all candidates from both interfaces.
             Dummy implementations don't conflict, though. *)
          S.at_most_one sat (our_vars @ replacements) |> ignore
        )
      )
    )

  (** On multi-arch systems, we can select 32-bit or 64-bit implementations,
      but not both in the same set of selections. *)
  module Machine_group = struct
    module Map = Map.Make(struct type t = Model.machine_group let compare = compare end)

    type t = {
      sat : S.t;
      mutable groups : S.lit Map.t;
    }

    let create sat = { sat; groups = Map.empty }

    let var t name =
      match Map.find_opt name t.groups with
      | Some v -> v
      | None ->
        let v = S.add_variable t.sat @@ SolverData.MachineGroup ("m." ^ (name :> string)) in
        t.groups <- Map.add name v t.groups;
        v

    (* If [impl] requires a particular machine group, add a constraint to the problem. *)
    let process t impl_var impl =
      Model.machine_group impl |> Option.iter (fun group ->
        S.implies t.sat ~reason:"machine group" impl_var [var t group]
      )

    (* Call this at the end to add the final clause with all discovered groups.
       [t] must not be used after this. *)
    let seal t =
      let xs = Map.bindings t.groups in
      if List.length xs > 1 then (
        (* If we get to the end of the solve without deciding then nothing we selected cares about the
           type of CPU. The solver will set them all to false at the end. *)
        S.at_most_one t.sat (List.map snd xs) |> ignore
      )
  end

  module Conflict_classes = struct
    module Map = Map.Make(struct type t = Model.conflict_class let compare = compare end)

    type t = {
      sat : S.t;
      mutable groups : S.lit list ref Map.t;
    }

    let create sat = { sat; groups = Map.empty }

    let var t name =
      match Map.find_opt name t.groups with
      | Some v -> v
      | None ->
        let v = ref [] in
        t.groups <- Map.add name v t.groups;
        v

    (* Add [impl] to its conflict groups, if any. *)
    let process t impl_var impl =
      Model.conflict_class impl |> List.iter (fun name ->
          let impls = var t name in
          impls := impl_var :: !impls
      )

    (* Call this at the end to add the final clause with all discovered groups.
       [t] must not be used after this. *)
    let seal t =
      t.groups |> Map.iter @@ fun _name impls ->
      let impls = !impls in
      if List.length impls > 1 then (
        S.at_most_one t.sat impls |> ignore
      )
  end

  (** If this binding depends on a command (<executable-in-*>), add that to the problem.
     @param user_var indicates when this binding is used
     @param dep_iface the required interface this binding targets *)
  let process_self_command sat lookup_command user_var dep_role name =
    (* Note: we only call this for self-bindings, so we could be efficient by selecting the exact command here... *)
    let+ candidates = lookup_command (name, dep_role) in
    S.implies sat ~reason:"binding on command" user_var candidates#get_vars

  (* Process a dependency of [user_var]:
     - find the candidate implementations/commands to satisfy it
     - take just those that satisfy any restrictions in the dependency
     - ensure that we don't pick an incompatbile version if we select [user_var]
     - ensure that we do pick a compatible version if we select [user_var] (for "essential" dependencies only) *)
  let process_dep sat lookup_impl lookup_command user_var dep : unit Monad.t =
    let { Model.dep_role; dep_importance; dep_required_commands } = Model.dep_info dep in
    let dep_restrictions = Model.restrictions dep in

    (* Restrictions on the candidates *)
    let meets_restrictions impl = List.for_all (Model.meets_restriction impl) dep_restrictions in
    let* candidates = lookup_impl dep_role in
    let pass, fail = candidates#partition meets_restrictions in

    (* Dependencies on commands *)
    let+ () =
    dep_required_commands |> Monad.List.iter (fun name ->
      let+ candidates = lookup_command (name, dep_role) in

      if dep_importance = `Essential then (
        S.implies sat ~reason:"dep on command" user_var candidates#get_vars
      ) else (
        (* An optional dependency is selected when any implementation of the target interface
         * is selected. Force [dep_iface_selected] to be true in that case. We only need to test
         * [pass] here, because we always avoid [fail] anyway. *)
        let dep_iface_selected = S.add_variable sat (SolverData.Role dep_role) in
        S.at_most_one sat (S.neg dep_iface_selected :: pass) |> ignore;

        (* If user_var is selected, then either we don't select this interface, or we select
         * a suitable command. *)
        S.implies sat ~reason:"opt dep on command" user_var (S.neg dep_iface_selected :: candidates#get_vars)
      );
    ) in

    if dep_importance = `Essential then (
      S.implies sat ~reason:"essential dep" user_var pass     (* Must choose a suitable candidate *)
    ) else (
      (* If [user_var] is selected, don't select an incompatible version of the optional dependency.
         We don't need to do this explicitly in the [essential] case, because we must select a good
         version and we can't select two. *)
      try
        S.at_most_one sat (user_var :: fail) |> ignore;
      with Invalid_argument reason ->   (* Explicitly conflicts with itself! *)
        S.at_least_one sat [S.neg user_var] ~reason
    )

  (* Add the implementations of an interface to the ImplCache (called the first time we visit it). *)
  let make_impl_clause sat ~dummy_impl replacements role =
    let+ {Model.replacement; impls} = Model.implementations role in

    (* Insert dummy_impl (last) if we're trying to diagnose a problem. *)
    let impls =
      match dummy_impl with
      | None -> impls
      | Some dummy_impl -> impls @ [dummy_impl] in

    let impls = impls
      |> List.map (fun impl ->
          let var = S.add_variable sat (SolverData.ImplElem impl) in
          (var, impl)
      ) in
    let impl_clause = if impls <> [] then Some (S.at_most_one sat (List.map fst impls)) else None in
    let clause = new impl_candidates role impl_clause impls dummy_impl in

    (* If we have a <replaced-by>, remember to add a conflict with our replacement *)
    replacement |> Option.iter (fun replacement ->
      replacements := (clause, replacement) :: !replacements;
    );

    clause, impls

  (* Create a new CommandCache entry (called the first time we request this key). *)
  let make_commands_clause sat lookup_impl process_self_commands process_deps key =
    let (command, role) = key in
    let+ impls = lookup_impl role in
    let commands = impls#get_commands command in
    let make_provides_command (_impl, elem) =
      (* [var] will be true iff this <command> is selected. *)
      let var = S.add_variable sat (SolverData.CommandElem elem) in
      (var, elem) in
    let vars = List.map make_provides_command commands in
    let command_clause = if vars <> [] then Some (S.at_most_one sat @@ List.map fst vars) else None in
    let data = new command_candidates role command_clause vars in

    (data, fun () ->
      let depend_on_impl (command_var, command) (impl_var, _command) =
        (* For each command, require that we select the corresponding implementation. *)
        S.implies sat ~reason:"impl for command" command_var [impl_var];
        let deps, self_commands = Model.command_requires role command in
        (* Commands can depend on other commands in the same implementation *)
        let* () = process_self_commands command_var role self_commands in
        (* Process command-specific dependencies *)
        process_deps command_var deps
      in
      Monad.List.iter2 depend_on_impl vars commands
    )

  (** Starting from [root_req], explore all the feeds, commands and implementations we might need, adding
   * all of them to [sat_problem]. *)
  let build_problem root_req sat ~dummy_impl =
    (* For each (iface, command, source) we have a list of implementations (or commands). *)
    let impl_cache = ImplCache.create () in
    let command_cache = CommandCache.create () in

    let machine_groups = Machine_group.create sat in
    let conflict_classes = Conflict_classes.create sat in

    (* Handle <replaced-by> conflicts after building the problem. *)
    let replacements = ref [] in

    let rec add_impls_to_cache role =
      let+ clause, impls = make_impl_clause sat ~dummy_impl replacements role in
      (clause, fun () ->
        impls |> Monad.List.iter (fun (impl_var, impl) ->
          Machine_group.process machine_groups impl_var impl;
          Conflict_classes.process conflict_classes impl_var impl;
          let deps, self_commands = Model.requires role impl in
          let* () = process_self_commands impl_var role self_commands in
          process_deps impl_var deps
        )
      )
    and add_commands_to_cache key = make_commands_clause sat lookup_impl process_self_commands process_deps key
    and lookup_impl key = ImplCache.lookup impl_cache add_impls_to_cache key
    and lookup_command key = CommandCache.lookup command_cache add_commands_to_cache key
    and process_self_commands user_var dep_role = Monad.List.iter (process_self_command sat lookup_command user_var dep_role)
    and process_deps user_var : _ -> unit Monad.t = Monad.List.iter (process_dep sat lookup_impl lookup_command user_var)
    in

    let+ () =
    (* This recursively builds the whole problem up. *)
    begin match root_req with
      | {Model.role; command = None} -> let+ impl = (lookup_impl role) in impl#get_vars
      | {Model.role; command = Some command} -> let+ command = (lookup_command (command, role)) in command#get_vars end
    >>| S.at_least_one sat ~reason:"need root" (* Must get what we came for! *)
    in 

    (* All impl_candidates and command_candidates have now been added, so snapshot the cache. *)
    let impl_clauses, command_clauses = ImplCache.snapshot impl_cache, CommandCache.snapshot command_cache in
    add_replaced_by_conflicts sat impl_clauses !replacements;
    Machine_group.seal machine_groups;
    Conflict_classes.seal conflict_classes;
    impl_clauses, command_clauses

  module Output = struct
    module Input = Model
    module Role = Input.Role
    module RoleMap = RoleMap

    type impl = selection
    type command = Model.command
    type command_name = Model.command_name
    type dependency = Model.dependency

    type dep_info = Model.dep_info = {
      dep_role : Role.t;
      dep_importance : [ `Essential | `Recommended | `Restricts ];
      dep_required_commands : command_name list;
    }

    type requirements = Model.requirements = {
      role : Role.t;
      command : command_name option;
    }

    let dep_info = Model.dep_info
    let requires role impl = Model.requires role impl.impl
    let command_requires role cmd = Model.command_requires role cmd
    let get_command impl name = Model.get_command impl.impl name

    type t = {
      root_req : requirements;
      selections : selection RoleMap.t;
    }

    let to_map t = t.selections
    let requirements t = t.root_req

    let explain t role =
      match RoleMap.find_opt role t.selections with
      | Some sel -> explain sel.diagnostics
      | None -> "Role not used!"

    let get_selected role t =
      match RoleMap.find_opt role t.selections with
      | Some selection when selection.impl == Model.dummy_impl -> None
      | x -> x

    let selected_commands sel = sel.commands
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

       In all cases, a dependency may be on an <implementation> or on a specific <command>.
     *)

    let sat = S.create () in
    let dummy_impl = if closest_match then Some Model.dummy_impl else None in
    let+ impl_clauses, command_clauses = build_problem root_req sat ~dummy_impl in

    let lookup = function
      | {Model.role; command = None} -> (ImplCache.get_exn role impl_clauses :> candidates)
      | {Model.role; command = Some command} -> (CommandCache.get_exn (command, role) command_clauses) in

    (* Run the solve *)

    let decider () =
      (* Walk the current solution, depth-first, looking for the first undecided interface.
         Then try the most preferred implementation of it that hasn't been ruled out. *)
      let seen = Hashtbl.create 100 in
      let rec find_undecided req =
        if Hashtbl.mem seen req then None    (* Break cycles *)
        else (
          Hashtbl.add seen req true;
          let candidates = lookup req in
          match candidates#get_state with
          | Unselected -> None
          | Undecided lit -> Some lit
          | Selected (deps, self_commands) ->
              (* We've already selected a candidate for this component. Now check its dependencies. *)
              let check_self_command name = find_undecided {req with Model.command = Some name} in
              match List.find_map check_self_command self_commands with
              | Some _ as r -> r
              | None ->
              (* Self-commands already done; now try the dependencies *)
              let check_dep dep =
                let { Model.dep_role; dep_importance; dep_required_commands } = Model.dep_info dep in
                if dep_importance = `Restricts then (
                  (* Restrictions don't express that we do or don't want the
                     dependency, so skip them here. If someone else needs this,
                     we'll handle it when we get to them.
                     If noone wants it, it will be set to unselected at the end. *)
                  None
                ) else (
                  match find_undecided {Model.role = dep_role; command = None} with
                  | Some lit -> Some lit
                  | None ->
                      (* Command dependencies next *)
                      let check_command_dep name = find_undecided {Model.command = Some name; role = dep_role} in
                      List.find_map check_command_dep dep_required_commands
                )
                in
              match List.find_map check_dep deps with
              | Some _ as r -> r
              | None ->
              (* All dependencies checked; now to the impl (if we're a <command>) *)
              Option.bind req.Model.command (fun _command -> find_undecided {req with Model.command = None})
        ) in
      find_undecided root_req in

    match S.run_solver sat decider with
    | None -> None
    | Some _solution ->
        (* Build the results object *)

        (* For each implementation, remember which commands we need. *)
        let commands_needed = Hashtbl.create 10 in
        command_clauses
        |> CommandCache.M.iter (fun (command_name, role) candidates ->
            candidates#get_clause |> Option.iter (fun clause ->
              if S.get_selected clause <> None then
                Hashtbl.add commands_needed role command_name
            )
        );

        let selections =
          impl_clauses
          |> ImplCache.filter_map (fun role candidates ->
              candidates#get_selected |> Option.map (fun (lit, impl) ->
                let commands = Hashtbl.find_all commands_needed role in
                {impl; commands; diagnostics = lit}
              )
          ) in
        Some { Output.root_req; selections }
end
