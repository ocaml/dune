(*
   This file is extracted from the 0install library. It is distributed under
   the LGPL-2.1-or-later licence. See src/sat/COPYING.md for the full license.

   Copyright (C) 2013, Thomas Leonard
   See the README file for details, or visit http://0install.net.
*)

(** A general purpose SAT solver. *)

(** The design of this solver is very heavily based on the one described in
    the MiniSat paper "An Extensible SAT-solver [extended version 1.2]"
    http://minisat.se/Papers.html

    The main differences are:

    - We care about which solution we find (not just "satisfiable" or "not").
    - We take care to be deterministic (always select the same versions given
      the same input). We do not do random restarts, etc.
    - We add an at_most_clause (the paper suggests this in the Excercises, and
      it's very useful for our purposes). *)

open Stdune

let debug = false

module type USER = sig
  type t

  val pp : t -> 'tag Pp.t
end

module VarID : sig
  type t
  type mint

  val to_dyn : t -> Dyn.t
  val compare : t -> t -> Ordering.t
  val make_mint : unit -> mint
  val issue : mint -> t

  module Hash_set : sig
    type id := t
    type t

    val create : unit -> t
    val mem : t -> id -> bool
    val add : t -> id -> unit
    val clear : t -> unit
  end
end = struct
  type t = int
  type mint = int ref

  let to_dyn = Dyn.int
  let compare (a : int) (b : int) = Int.compare a b
  let make_mint () = ref 0

  let issue mint =
    let i = !mint in
    incr mint;
    i
  ;;

  module Hash_set = Hash_set
end

module Var_value = struct
  type t =
    | True
    | False
    | Undecided

  let to_string = function
    | True -> "true"
    | False -> "false"
    | Undecided -> "undecided"
  ;;

  let to_dyn t = Dyn.variant (to_string t) []

  let invert = function
    | True -> False
    | False -> True
    | Undecided -> Undecided
  ;;
end

type sign =
  | Pos
  | Neg

open Pp.O

let log_debug p =
  Pp.to_fmt Format.std_formatter (Pp.vbox (Pp.hovbox (Pp.text "sat: " ++ p)) ++ Pp.cut)
;;

module Make (User : USER) = struct
  type clause =
    | Union of lit array
    | At_most of at_most_clause

  and at_most_clause = int * lit list ref * lit array
  (* max number [nb] of literals that can be True,
     list of the currently selected True literals (of length less than [nb]),
     array of all watched literals *)

  and at_most_one_clause = at_most_clause (* special case with [nb] enforced to be 1 *)

  (** The reason why a literal is set. *)
  and reason =
    | Clause of clause (* the clause that caused this literal to be true *)
    | External of string (* set externally (input fact or decider choice) *)

  and undo =
    | Undo_at_most of lit list ref
    | Decided

  and var =
    { id : VarID.t (* A unique ID, used to test identity *)
    ; mutable value : Var_value.t (* True/False/Undecided *)
    ; mutable reason : reason option
      (* The constraint that implied our value, if True or False *)
    ; mutable level : int
      (* The decision level at which we got a value (when not Undecided) *)
    ; mutable undo : undo list
      (* Functions to call if we become unbound (by backtracking) *)
    ; watch_queue : clause Queue.t (* Clauses to notify when var becomes True *)
    ; neg_watch_queue : clause Queue.t (* Clauses to notify when var becomes False *)
    ; obj : User.t (* The object this corresponds to (for our caller and for debugging) *)
    }

  and lit = sign * var

  type t =
    { id_maker : VarID.mint
    ; (* Propagation *)
      mutable vars : var list
    ; propQ : lit Queue.t (* propagation queue *)
    ; (* Assignments *)
      mutable trail : lit list (* order of assignments, most recent first *)
    ; mutable trail_len : int
    ; mutable trail_lim : int list (* decision levels (len(trail) at each decision) *)
    ; mutable trail_lim_len : int
    ; mutable toplevel_conflict : bool
    ; mutable set_to_false : bool
    ; conflict_vars : VarID.Hash_set.t
      (* we are finishing up by setting everything else to False *)
    }

  let lit_equal (s1, v1) (s2, v2) = s1 == s2 && v1 == v2

  module C = Comparable.Make (VarID)

  module LitSet = struct
    type t =
      { pos : VarID.Hash_set.t
      ; neg : VarID.Hash_set.t
      }

    let create () = { pos = VarID.Hash_set.create (); neg = VarID.Hash_set.create () }
    let temp = create ()

    let clear () =
      VarID.Hash_set.clear temp.pos;
      VarID.Hash_set.clear temp.neg
    ;;

    let mem { pos; neg } (sign, lit) =
      match sign with
      | Pos -> VarID.Hash_set.mem pos lit.id
      | Neg -> VarID.Hash_set.mem neg lit.id
    ;;

    let add { pos; neg } (sign, lit) =
      match sign with
      | Pos -> VarID.Hash_set.add pos lit.id
      | Neg -> VarID.Hash_set.add neg lit.id
    ;;
  end

  let make_var id obj =
    { id
    ; value = Undecided
    ; reason = None
    ; level = -1
    ; undo = []
    ; watch_queue = Queue.create ()
    ; neg_watch_queue = Queue.create ()
    ; obj
    }
  ;;

  let neg = function
    | Pos, var -> Neg, var
    | Neg, var -> Pos, var
  ;;

  let var_of_lit (_, var) = var

  let watch_queue = function
    | Pos, var -> var.watch_queue
    | Neg, var -> var.neg_watch_queue
  ;;

  exception ConflictingClause of clause
  exception SolveDone of bool

  type added_result =
    | AddedFact of bool (* Result of enqueing the new fact *)
    | AddedClause of clause

  let create () =
    if debug then log_debug (Pp.text "--- new SAT problem ---");
    { id_maker = VarID.make_mint ()
    ; vars = []
    ; propQ = Queue.create ()
    ; trail = []
    ; trail_len = 0
    ; trail_lim = []
    ; trail_lim_len = 0
    ; toplevel_conflict = false
    ; set_to_false = false
    ; conflict_vars = VarID.Hash_set.create ()
    }
  ;;

  (* For nicer log_debug messages *)
  let name_lit (sign, var) =
    match sign with
    | Pos -> User.pp var.obj
    | Neg -> Pp.text "not(" ++ User.pp var.obj ++ Pp.char ')'
  ;;

  let undo problem undo lit =
    match undo with
    | Decided -> problem.set_to_false <- false
    | Undo_at_most current ->
      if debug
      then
        log_debug
          (Pp.text "(backtracking: no longer selected " ++ name_lit lit ++ Pp.char ')');
      (match !current with
       | l :: lst ->
         assert (lit_equal lit l);
         current := lst
       | [] -> assert false)
  ;;

  let pp_lits lits = Pp.concat_map ~sep:(Pp.text ", ") lits ~f:name_lit

  let lit_value (sign, var) =
    match sign with
    | Pos -> var.value
    | Neg -> Var_value.invert var.value
  ;;

  let get_user_data_for_lit lit = (var_of_lit lit).obj

  let pp_lit_assignment l =
    let info = var_of_lit l in
    User.pp info.obj ++ Pp.textf "=%s" (Var_value.to_string info.value)
  ;;

  let pp_clause = function
    | Union lits ->
      Pp.text "<some: "
      ++ Pp.concat_map ~sep:(Pp.text ", ") ~f:name_lit (Array.to_list lits)
      ++ Pp.char '>'
    | At_most (nb, _, lits) ->
      Pp.textf "<at most %i: " nb ++ pp_lits (Array.to_list lits) ++ Pp.char '>'
  ;;

  let pp_reason = function
    | Clause clause -> pp_clause clause
    | External msg -> Pp.text msg
  ;;

  let get_decision_level problem = problem.trail_lim_len

  let add_variable problem obj : lit =
    (* if debug then log_debug "add_variable('%s')" obj; *)
    let var =
      let i = VarID.issue problem.id_maker in
      make_var i obj
    in
    problem.vars <- var :: problem.vars;
    Pos, var
  ;;

  (* [lit] is now [True].
     [reason] is the clause that is asserting this.
     @return [false] if this immediately causes a conflict. *)
  let enqueue problem lit reason =
    if debug
    then
      log_debug
        (Pp.text "enqueue: "
         ++ name_lit lit
         ++ Pp.text " ("
         ++ pp_reason reason
         ++ Pp.char ')');
    match lit_value lit with
    | False -> false (* Conflict *)
    | True -> true (* Already set (shouldn't happen) *)
    | Undecided ->
      let var_info = var_of_lit lit in
      var_info.value <- (if fst lit == Neg then False else True);
      var_info.level <- get_decision_level problem;
      var_info.reason <- Some reason;
      problem.trail <- lit :: problem.trail;
      problem.trail_len <- problem.trail_len + 1;
      Queue.push problem.propQ lit;
      true
  ;;

  (* Pop most recent assignment from [trail] *)
  let undo_one problem =
    match problem.trail with
    | [] -> assert false
    | lit :: rest ->
      (* if debug then log_debug "(pop %s)" (name_lit lit); *)
      let var_info = var_of_lit lit in
      var_info.value <- Undecided;
      var_info.reason <- None;
      var_info.level <- -1;
      problem.trail <- rest;
      problem.trail_len <- problem.trail_len - 1;
      while var_info.undo <> [] do
        let cb = List.hd var_info.undo in
        var_info.undo <- List.tl var_info.undo;
        undo problem cb lit
      done
  ;;

  let cancel problem =
    let n_this_level = problem.trail_len - List.hd problem.trail_lim in
    if debug
    then
      log_debug
        (Pp.textf
           "backtracking from level %d (%d assignments)"
           (get_decision_level problem)
           n_this_level);
    for _i = 1 to n_this_level do
      undo_one problem
    done;
    problem.trail_lim <- List.tl problem.trail_lim;
    problem.trail_lim_len <- problem.trail_lim_len - 1
  ;;

  let cancel_until problem level =
    while get_decision_level problem > level do
      cancel problem
    done
  ;;

  let impossible problem = problem.toplevel_conflict <- true

  (* Call [Clause.propagate lit] when lit becomes True *)
  let watch_lit lit clause =
    (* if debug then log_debug "%s is watching for %s to become True" clause#to_string (name_lit lit); *)
    Queue.push (watch_queue lit) clause
  ;;

  exception Conflict

  module Clause = struct
    (* Why are we causing a conflict?
       @return a list of literals which caused the problem by all being True. *)
    let calc_reason = function
      | Union lits -> List.map ~f:neg (Array.to_list lits)
      | At_most (nb, _, lits) ->
        (* If we caused a conflict, it's because nb+1 of our literals became true. *)
        (* Find the nb+1 True literals *)
        let rec find nb_founds founds lits i =
          if Int.equal i (Array.length lits)
          then assert false (* Don't know why! *)
          else (
            let x = lits.(i) in
            if lit_value x <> True
            then find nb_founds founds lits (i + 1)
            else if nb_founds >= nb
            then x :: founds
            else find (nb_founds + 1) (x :: founds) lits (i + 1))
        in
        find 0 [] lits 0
    ;;

    (* Which literals caused [lit] to have its current value?
       @return a list of literals which caused the problem by all being True. *)
    let calc_reason_for t lit =
      match t with
      | Union lits ->
        (* Which literals caused [lit] to have its current value? *)
        assert (lit_equal lit lits.(0));
        (* The cause is everything except lit. *)
        let rec get_cause i =
          if i = Array.length lits
          then []
          else (
            let l = lits.(i) in
            if lit_equal l lit then get_cause (i + 1) else neg l :: get_cause (i + 1))
        in
        get_cause 0
      | At_most (_, _, lits) ->
        (* Find the True literals. All true literal other than [lit] would do. *)
        Array.to_list lits
        |> List.filter ~f:(fun l -> (not (lit_equal l lit)) && lit_value l = True)
    ;;

    (* [lit] is now [True]. Add any new deductions. @return false if there is a
       conflict. *)
    let propagate problem t lit =
      match t with
      | Union lits ->
        (* Try to infer new facts.
           We can do this only when all of our literals are False except one,
           which is undecided. That is,
           False... or X or False... = True  =>  X = True

           To get notified when this happens, we tell the solver to
           watch two of our undecided literals. Watching two undecided
           literals is sufficient. When one changes we check the state
           again. If we still have two or more undecided then we switch
           to watching them, otherwise we propagate.

           Returns false on conflict. *)
        (* [neg lit] has just become False *)

        (*if debug then log_debug("%s: noticed %s has become False" % (self, self.solver.name_lit(neg(lit)))) *)

        (* For simplicity, only handle the case where self.lits[1]
           is the one that just got set to False, so that:
           - value[lits[0]] = Undecided | True
           - value[lits[1]] = False
             If it's the other way around, just swap them before we start. *)
        if lit_equal lits.(0) (neg lit) then Array.swap lits 0 1;
        if lit_value lits.(0) = True
        then (
          (* We're already satisfied. Do nothing. *)
          watch_lit lit t;
          true)
        else (
          assert (lit_value lits.(1) = False);
          (* Find a new literal to watch now that lits[1] is resolved, *)
          (* swap it with lits[1], and start watching it. *)
          let rec find_not_false i =
            if i = Array.length lits
            then (
              (* Only lits[0], is now undefined, so set it to True. *)
              watch_lit lit t;
              enqueue problem lits.(0) (Clause t))
            else (
              match lit_value lits.(i) with
              | False -> find_not_false (i + 1)
              | Undecided | True ->
                (* If it's True then we've already done our job,
                   so this means we don't get notified unless we backtrack, which is fine. *)
                Array.swap lits 1 i;
                watch_lit (neg lits.(1)) t;
                true)
          in
          find_not_false 2)
      | At_most (nb, current, lits) ->
        (* Re-add ourselves to the watch list.
           (we we won't get any more notifications unless we backtrack,
           in which case we'd need to get back on the list anyway) *)
        watch_lit lit t;
        (* value[lit] has just become true *)
        assert (lit_value lit = True);
        (* if debug then log_debug("%s: noticed %s has become True" % (self, self.solver.name_lit(lit))) *)

        (* If we already propagated successfully when the [nb]
           one was set then we set all the others to false and
           anyone trying to set one true will get rejected. And
           if we didn't propagate yet, current will still be
           incomplete, even if we now have a conflict (which we'll
           detect below). When [nb = 0], the constraint will fail
           immediately. *)
        assert (List.length !current < nb || nb = 0);
        current := lit :: !current;
        (let var_info = var_of_lit lit in
         (* If we later backtrack, unset current *)
         var_info.undo <- Undo_at_most current :: var_info.undo);
        let nb_true = List.length !current in
        if nb_true < nb
        then true
        else if nb_true > nb
        then (
          assert (nb = 0);
          false)
        else (
          try
            let clause = Clause t in
            (* We set all other literals to False. *)
            let nb_true = ref 0 in
            Array.iter lits ~f:(fun l ->
              match lit_value l with
              | True ->
                incr nb_true;
                (* Due to queuing, more literals could have been selected
                   before we got called. *)
                if !nb_true > nb
                then (
                  if debug
                  then log_debug (Pp.text "CONFLICT: already selected " ++ name_lit l);
                  raise_notrace Conflict)
              | Undecided ->
                (* Since we have the right number of lits selected,
                   all unknown ones can be set to False. *)
                if not (enqueue problem (neg l) clause)
                then (
                  if debug
                  then
                    log_debug (Pp.text "CONFLICT: enqueue failed for " ++ name_lit (neg l));
                  raise_notrace
                    Conflict (* Can't happen, since we already checked we're Undecided *))
              | False -> ());
            true
          with
          | Conflict -> false)
    ;;
  end

  (** Process the propQ.
      Returns None when done, or the clause that caused a conflict. *)
  let propagate problem =
    (* if debug then log_debug "propagate: queue length = %d" (Queue.length problem.propQ); *)
    try
      while not (Queue.is_empty problem.propQ) do
        let lit = Queue.pop_exn problem.propQ in
        let old_watches = Queue.create () in
        let watches = watch_queue lit in
        Queue.transfer watches old_watches;
        (* if debug then log_debug "%s -> True : watches: %d" (name_lit lit) (Queue.length old_watches); *)

        (* Notify all watchers *)
        while not (Queue.is_empty old_watches) do
          let clause = Queue.pop_exn old_watches in
          if not (Clause.propagate problem clause lit)
          then (
            (* Conflict *)

            (* Re-add remaining watches *)
            Queue.transfer old_watches watches;
            (* No point processing the rest of the queue as
               we'll have to backtrack now. *)
            Queue.clear problem.propQ;
            raise_notrace (ConflictingClause clause))
        done
      done;
      None
    with
    | ConflictingClause c -> Some c
  ;;

  let get_best_undecided (_, _, lits) =
    (* if debug then log_debug "best_undecided: %s" (string_of_lits lits); *)
    Array.find_opt lits ~f:(fun l -> lit_value l = Undecided)
  ;;

  let get_selected (_, current, _) =
    match !current with
    | [] -> None
    | [ single ] -> Some single
    | _ ->
      (* Impossible given the types in the mli *)
      failwith "Sat.get_selected: too many for an at_most_one_clause"
  ;;

  (* Returns the new clause if one was added, [AddedFact true] if none was added
     because this clause is trivially True, or [AddedFact false] if the clause
     caused a conflict. *)
  let internal_at_least_one problem lits ~learnt ~reason =
    match lits with
    | [] -> assert false
    | [ lit ] ->
      (* A clause with only a single literal is represented
         as an assignment rather than as a clause. *)
      AddedFact (enqueue problem lit reason)
    | lits ->
      let lits = Array.of_list lits in
      if learnt
      then (
        (* lits[0] is Undecided because we just backtracked.
           Start watching the next literal that we will backtrack over. *)
        let best_level = ref (-1) in
        let best_i = ref 1 in
        for i = 0 to Array.length lits - 1 do
          let lit = lits.(i) in
          let level = (var_of_lit lit).level in
          if level > !best_level
          then (
            best_level := level;
            best_i := i)
        done;
        Array.swap lits 1 !best_i);
      let clause = Union lits in
      (* Watch the first two literals in the clause (both must be
         undefined at this point). *)
      let watch i = watch_lit (neg lits.(i)) clause in
      watch 0;
      watch 1;
      AddedClause clause
  ;;

  let simplify lits =
    let seen =
      LitSet.clear ();
      LitSet.temp
    in
    let rec simplify unique = function
      | [] -> Some unique
      | x :: _ when LitSet.mem seen (neg x) -> None (* X or not(X) is always True *)
      | x :: xs when LitSet.mem seen x -> simplify unique xs (* Skip duplicates *)
      | x :: xs when lit_value x = False ->
        simplify unique xs (* Skip values known to be False *)
      | x :: xs ->
        LitSet.add seen x;
        simplify (x :: unique) xs
    in
    simplify [] lits
  ;;

  (** Public interface. Only used before the solve starts. *)
  let at_least_one problem ?(reason = "input fact") lits =
    if List.is_empty lits
    then problem.toplevel_conflict <- true
    else if
      (* if debug then log_debug "at_least_one(%s)" (string_of_lits lits); *)
      List.exists lits ~f:(fun l -> lit_value l = True)
    then (* Trivially true already if any literal is True. *)
      ()
    else (
      (* At this point, [unique] contains only [Undefined] literals. *)
      match simplify lits with
      | None -> ()
      | Some [] ->
        problem.toplevel_conflict <- true (* Everything in the list was False *)
      | Some unique ->
        if
          internal_at_least_one problem unique ~learnt:false ~reason:(External reason)
          = AddedFact false
        then problem.toplevel_conflict <- true)
  ;;

  let implies problem ?reason first rest = at_least_one problem ?reason (neg first :: rest)

  let has_duplicates lits =
    LitSet.clear ();
    let seen = LitSet.temp in
    try
      List.iter lits ~f:(fun lit ->
        if LitSet.mem seen lit then raise_notrace Exit else LitSet.add seen lit);
      false
    with
    | Exit -> true
  ;;

  let at_most nb lits : at_most_clause =
    assert (not (List.is_empty lits));
    (* if debug then log_debug (Pp.textf "at_most_%i(%s)" nb (string_of_lits lits)); *)
    (* If there are not enough literals then we're trivially true
       and not really needed for the solve. However, Zero Install
       monitors these objects to find out what was selected, so
       keep even trivial ones around for that.
    *)
    (*if len(lits) <= nb: *)
    (*	return True	# Trivially true *)

    (* Ensure no duplicates *)
    if has_duplicates lits
    then (
      let msg =
        Pp.text "at_most_one(" ++ pp_lits lits ++ Pp.paragraph "): duplicates in list!"
      in
      invalid_arg (Format.asprintf "%a." Pp.to_fmt msg));
    (* Ignore any literals already known to be False.
       If any are True then they're enqueued and we'll process them
       soon. *)
    let lits = List.filter lits ~f:(fun l -> lit_value l <> False) |> Array.of_list in
    let clause = nb, ref [], lits in
    (let clause = At_most clause in
     Array.iter lits ~f:(fun l -> watch_lit l clause));
    clause
  ;;

  let at_most_one lits = at_most 1 lits

  let analyse problem (original_cause : clause) =
    (* After trying some assignments, we've discovered a conflict.
       e.g.
       - we selected A then B then C
       - from A, B, C we got X, Y
       - we have a rule (original_cause): not(A) or not(X) or not(Y)

       The simplest thing to do would be:
       1. add the rule "not(A) or not(B) or not(C)"
       2. unassign C

       Then we'd deduce not(C) and we could try something else.
       However, that would be inefficient. We want to learn a more
       general rule that will help us with the rest of the problem.

       We take the clause that caused the conflict (original_cause)
       and ask it for its cause. In this case:

       A and X and Y => conflict

       Since X and Y followed logically from A, B, C there's no
       point learning this rule; we need to know to avoid A, B, C
       *before* choosing C. We ask the two variables deduced at the
       current level (X and Y) what caused them, and work backwards.
       e.g.

       X: A and C => X
       Y: C => Y

       Combining these, we get the cause of the conflict in terms of
       things we knew before the current decision level:

       A and X and Y => conflict
       A and (A and C) and (C) => conflict
       A and C => conflict

       We can then learn (record) the more general rule:

       not(A) or not(C)

       Then, in future, whenever A is selected we can remove C and
       everything that depends on it from consideration.
    *)
    let learnt = ref [] in
    (* The general rule we're learning *)
    let btlevel = ref 0 in
    (* The deepest decision in learnt *)
    let seen =
      VarID.Hash_set.clear problem.conflict_vars;
      problem.conflict_vars
    in
    (* The variables involved in the conflict *)
    let counter = ref 0 in
    (* The number of pending variables to check *)
    (* [outcome] was caused by the literals [p_reason] all being True. Follow the
       causes back, adding anything decided before this level to [learnt]. When
       we get bored, return the literal we were processing at the time. *)
    let rec follow_causes p_reason outcome =
      if debug
      then
        log_debug
          (Pp.text "because "
           ++ Pp.concat_map ~sep:(Pp.text " and ") ~f:name_lit p_reason
           ++ Pp.text " => "
           ++ outcome);
      (* p_reason is in the form (A and B and ...) *)

      (* Check each of the variables in p_reason that we haven't
         already considered:
         - if the variable was assigned at the current level,
           mark it for expansion
         - otherwise, add it to learnt *)
      List.iter p_reason ~f:(fun lit ->
        let var = var_of_lit lit in
        if not (VarID.Hash_set.mem seen var.id)
        then (
          VarID.Hash_set.add seen var.id;
          let var_info = var_of_lit lit in
          if var_info.level = get_decision_level problem
          then
            (* We deduced this var since the last decision. It must be in
               [trail], so we'll get to it soon. Remember not to stop until
               we've processed it. *)
            (* if debug then log_debug "(will look at %s soon)" (name_lit lit); *)
            incr counter
          else if var_info.level > 0
          then (
            (* We won't expand lit, just remember it.
               (we could expand it if it's not a decision, but apparently not doing so is useful) *)
            (* if debug then log_debug "Can't follow %s past a decision point" (name_lit lit); *)
            learnt := neg lit :: !learnt;
            btlevel := max !btlevel var_info.level)
          (* else if debug then log_debug "Input fact: %s" (name_lit lit) *))
        (* else we already considered the cause of this assignment *));
      (* At this point, counter is the number of assigned
         variables in [trail] at the current decision level that
         we've seen. That is, the number left to process. Pop
         the next one off [trail] (as well as any unrelated
         variables before it; everything up to the previous
         decision has to go anyway). *)

      (* On the first time round the loop, we must find the
         conflict depends on at least one assignment at the
         current level. Otherwise, simply setting the decision
         variable caused a clause to conflict, in which case
         the clause should have asserted not(decision-variable)
         before we ever made the decision.
         On later times round the loop, [counter] was already >
         0 before we started iterating over [p_reason]. *)
      assert (!counter > 0);
      (* Pop assignments until we find one we care about. *)
      let rec next_interesting () =
        let lit = List.hd problem.trail in
        let var = var_of_lit lit in
        let reason = var.reason in
        undo_one problem;
        if VarID.Hash_set.mem seen var.id
        then reason, lit
        else
          (* if debug then log_debug "(irrelevant: %s)" (name_lit lit); *)
          next_interesting ()
      in
      let reason, p = next_interesting () in
      (* [reason] is the reason why [p] is True (i.e. it enqueued it). *)
      (* [p] is the literal we want to expand now. *)
      decr counter;
      if !counter <= 0
      then
        (* If counter = 0 then we still have one more literal (p) at the
           current level that we could expand. However, apparently it's best to
           leave this unprocessed (says the minisat paper). *)
        p
      else (
        let cause =
          match reason with
          | Some (Clause c) -> c
          | Some (External msg) ->
            Code_error.raise "external" [ "msg", Dyn.string msg ] (* Can't happen *)
          | None -> Code_error.raise "No reason!" []
        in
        (* Can't happen *)
        let p_reason = Clause.calc_reason_for cause p in
        let outcome = name_lit p in
        if debug
        then
          log_debug
            (Pp.text "why did "
             ++ pp_clause cause
             ++ Pp.text " lead to "
             ++ outcome
             ++ Pp.char '?');
        follow_causes p_reason outcome)
    in
    (* Start with all the literals involved in the conflict. *)
    if debug
    then
      log_debug
        (Pp.text "why did " ++ pp_clause original_cause ++ Pp.text " lead to conflict?");
    let p = follow_causes (Clause.calc_reason original_cause) (Pp.text "conflict") in
    assert (!counter = 0);
    (* p is the literal we decided to stop processing on. It's either
       a derived variable at the current level, or the decision that
       led to this level. Since we're not going to expand it, add it
       directly to the learnt clause. *)
    let learnt = neg p :: !learnt in
    if debug
    then
      log_debug
        (Pp.text "learnt: " ++ Pp.concat_map ~sep:(Pp.text " or ") ~f:name_lit learnt);
    learnt, !btlevel
  ;;

  let run_solver problem decide =
    (* Check whether we detected a trivial problem during setup. *)
    if problem.toplevel_conflict
    then (
      if debug then log_debug (Pp.text "FAIL: toplevel_conflict before starting solve!");
      false)
    else (
      try
        while true do
          (* Use logical deduction to simplify the clauses
             and assign literals where there is only one possibility. *)
          match propagate problem with
          | None ->
            (* No conflicts *)
            (* if debug then log_debug "new state: %s" problem.assigns *)

            (* Pick a variable and try assigning it one way.
               If it leads to a conflict, we'll backtrack and
               try it the other way. *)
            let undecided =
              match List.find problem.vars ~f:(fun info -> info.value = Undecided) with
              | Some s -> s
              | None -> raise_notrace (SolveDone true)
            in
            let lit =
              if problem.set_to_false
              then (* Printf.printf "%s -> false\n" (name_lit undecided); *)
                Neg, undecided
              else (
                match decide () with
                | Some lit -> lit
                | None ->
                  (* Switch to set_to_false mode (until we backtrack). *)
                  problem.set_to_false <- true;
                  undecided.undo <- Decided :: undecided.undo;
                  (* Printf.printf "%s -> false\n" (name_lit undecided); *)
                  Neg, undecided)
            in
            if debug then log_debug (Pp.text "TRYING: " ++ name_lit lit);
            let old = lit_value lit in
            if old <> Undecided
            then
              Code_error.raise
                "Decider chose already-decided variable"
                [ "lit", Dyn.string (Format.asprintf "%a@." Pp.to_fmt (name_lit lit))
                ; "was", Var_value.to_dyn old
                ];
            problem.trail_lim <- problem.trail_len :: problem.trail_lim;
            problem.trail_lim_len <- problem.trail_lim_len + 1;
            let r = enqueue problem lit (External "considering") in
            assert r
          | Some conflicting_clause ->
            if get_decision_level problem = 0
            then (
              if debug then log_debug (Pp.text "FAIL: conflict found at top level");
              raise_notrace (SolveDone false))
            else (
              (* Figure out the root cause of this failure. *)
              let learnt, backtrack_level = analyse problem conflicting_clause in
              (* We have learnt that something in [learnt] must be True or we get a conflict. *)
              cancel_until problem backtrack_level;
              match
                internal_at_least_one
                  problem
                  learnt
                  ~learnt:true
                  ~reason:(Clause conflicting_clause)
              with
              | AddedFact true -> ()
              | AddedFact false ->
                (* This is what the Python would do. Perhaps it can't happen? *)
                let e = enqueue problem (List.hd learnt) (External "conflict!") in
                assert e
              | AddedClause c ->
                (* Everything except the first literal in learnt is known to
                   be False, so the first must be True. *)
                let e = enqueue problem (List.hd learnt) (Clause c) in
                assert e)
        done;
        Code_error.raise "not reached" []
      with
      | SolveDone t -> t)
  ;;

  let pp_lit_reason lit =
    match (var_of_lit lit).reason with
    | None -> Pp.text "no reason (BUG)"
    | Some (External reason) -> Pp.text reason
    | Some (Clause c) ->
      let reason = Clause.calc_reason_for c lit in
      Pp.concat_map ~sep:(Pp.text " && ") reason ~f:pp_lit_assignment
  ;;

  (* Why is [lit] assigned the way it is? For debugging. *)
  let explain_reason lit =
    let value = lit_value lit in
    if value = Undecided
    then Pp.text "undecided!"
    else Pp.hovbox (pp_lit_reason lit ++ Pp.text " => " ++ pp_lit_assignment lit)
  ;;
end
