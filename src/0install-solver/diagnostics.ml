(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Explaining why a solve failed or gave an unexpected answer. *)

module List = Stdune.List

module Make (Results : S.SOLVER_RESULT) = struct
  open Fiber.O
  open Pp.O
  module Model = Results.Input
  module RoleMap = Results.RoleMap

  let format_role = Model.Role.pp

  let format_restrictions r =
    String.concat ", " (List.map ~f:Model.string_of_restriction r)
  ;;

  module Note = struct
    type t =
      | UserRequested of Model.restriction
      | ReplacesConflict of Model.Role.t
      | ReplacedByConflict of Model.Role.t
      | Restricts of Model.Role.t * Model.impl * Model.restriction list
      | Feed_problem of string

    let pp = function
      | UserRequested r -> Pp.paragraphf "User requested %s" (format_restrictions [ r ])
      | ReplacesConflict old ->
        Pp.hovbox (Pp.text "Replaces (and therefore conflicts with) " ++ format_role old)
      | ReplacedByConflict replacement ->
        Pp.hovbox
          (Pp.text "Replaced by (and therefore conflicts with) "
           ++ format_role replacement)
      | Restricts (other_role, impl, r) ->
        Pp.hovbox
          ~indent:2
          (format_role other_role
           ++ Pp.char ' '
           ++ Model.pp_version impl
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
      [ `Model_rejection of Model.rejection
      | `FailsRestriction of Model.restriction
      | `DepFailsRestriction of Model.dependency * Model.restriction
      | `ClassConflict of Model.Role.t * Model.conflict_class
      | `ConflictsRole of Model.Role.t
      | `DiagnosticsFailure of Stdune.User_message.Style.t Pp.t
      ]
    (* Why a particular implementation was rejected. This could be because the model rejected it,
       or because it conflicts with something else in the example (partial) solution. *)

    type reject = Model.impl * rejection_reason

    type t =
      { role : Model.Role.t
      ; replacement : Model.Role.t option
      ; diagnostics : Stdune.User_message.Style.t Pp.t Lazy.t
      ; selected_impl : Model.impl option
      ; (* orig_good is all the implementations passed to the SAT solver (these are the
           ones with a compatible OS, CPU, etc). They are sorted most desirable first. *)
        orig_good : Model.impl list
      ; orig_bad : (Model.impl * Model.rejection) list
      ; mutable good : Model.impl list
      ; mutable bad : (Model.impl * rejection_reason) list
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
      (selected_impl : Model.impl option)
      =
      let { Model.impls; Model.replacement } = candidates in
      let notes = List.map ~f:(fun x -> Note.Feed_problem x) feed_problems in
      { role
      ; replacement
      ; orig_good = impls
      ; orig_bad
      ; good = impls
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
      | Some selected when Model.compare_version selected impl > 0 -> false
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
          !n
          |> Option.iter (fun info ->
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
          if Model.meets_restriction impl r then None else Some (`FailsRestriction r)))
    ;;

    let apply_user_restriction t r =
      note t (UserRequested r);
      (* User restrictions should be applied before reaching the solver, but just in case: *)
      filter_impls t (fun impl ->
        if Model.meets_restriction impl r then None else Some (`FailsRestriction r));
      (* Completely remove non-matching impls.
         The user will only want to see the version they asked for. *)
      let new_bad =
        List.filter t.bad ~f:(fun (impl, _) ->
          if Model.meets_restriction impl r then true else false)
      in
      if new_bad <> [] || t.good <> [] then t.bad <- new_bad
    ;;

    let reject_all t reason =
      t.bad <- List.map ~f:(fun impl -> impl, reason) t.good @ t.bad;
      t.good <- []
    ;;

    let replacement t = t.replacement
    let selected_impl t = t.selected_impl

    (* When something conflicts with itself then our usual trick of selecting
       the main implementation and failing the dependency doesn't work, so
       special-case that here. *)
    let reject_self_conflicts t =
      filter_impls t (fun impl ->
        let deps = Model.requires t.role impl in
        List.find_map deps ~f:(fun dep ->
          let { Model.dep_role; _ } = Model.dep_info dep in
          if Model.Role.compare dep_role t.role <> 0
          then None
          else
            (* It depends on itself. *)
            Model.restrictions dep
            |> List.find_map ~f:(fun r ->
              if Model.meets_restriction impl r
              then None
              else Some (`DepFailsRestriction (dep, r)))))
    ;;

    let finalise t =
      if t.selected_impl = None
      then (
        reject_self_conflicts t;
        reject_all t (`DiagnosticsFailure (Lazy.force t.diagnostics)))
    ;;

    let pp_reject ((impl, reason) : reject) =
      match reason with
      | `Model_rejection r -> Model.describe_problem impl r
      | `FailsRestriction r ->
        Pp.paragraphf "Incompatible with restriction: %s" (Model.string_of_restriction r)
      | `DepFailsRestriction (dep, restriction) ->
        let dep_info = Model.dep_info dep in
        Pp.hovbox
          (Pp.text "Requires "
           ++ format_role dep_info.Model.dep_role
           ++ Pp.textf " %s" (format_restrictions [ restriction ]))
      | `ClassConflict (other_role, cl) ->
        Pp.hovbox
          (Pp.textf "In same conflict class (%s) as " (cl :> string)
           ++ format_role other_role)
      | `ConflictsRole other_role ->
        Pp.hovbox (Pp.text "Conflicts with " ++ format_role other_role)
      | `DiagnosticsFailure msg ->
        Pp.hovbox (Pp.text "Reason for rejection unknown: " ++ msg)
    ;;

    let show_rejections ~verbose rejected =
      let by_version (a, _) (b, _) =
        Model.compare_version b a |> Stdune.Ordering.of_int
      in
      let rejected = List.sort ~compare:by_version rejected in
      let rec aux i = function
        | [] -> Pp.nop
        | _ when i = 5 && not verbose -> Pp.cut ++ Pp.text "..."
        | (impl, problem) :: xs ->
          Pp.cut
          ++ Pp.hovbox
               ~indent:2
               (Model.pp_impl_long impl ++ Pp.text ": " ++ pp_reject (impl, problem))
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
      | Some sel -> Model.pp_impl_long sel
      | None -> Pp.text "(problem)"
    ;;

    (* Format a textual description of this component's report. *)
    let pp ~verbose t =
      Pp.vbox
        ~indent:2
        (Pp.hovbox (format_role t.role ++ Pp.text " -> " ++ pp_outcome t)
         ++ pp_notes t
         ++ pp_candidates ~verbose t)
    ;;
  end

  type t = Component.t RoleMap.t

  let find_component_ex role report =
    match RoleMap.find_opt role report with
    | Some c -> c
    | None ->
      Stdune.User_error.raise
        [ Pp.text "Can't find component " ++ format_role role ++ Pp.char '!' ]
  ;;

  (* Did any dependency of [impl] prevent it being selected?
     This can only happen if a component conflicts with something more important
     than itself (otherwise, we'd select something in [impl]'s interface and
     complain about the dependency instead).

     e.g. A depends on B and C. B and C both depend on D.
     C1 conflicts with D1. The depth-first priority order means we give priority
     to {A1, B1, D1}. Then we can't choose C1 because we prefer to keep D1. *)
  let get_dependency_problem role report impl =
    let check_dep dep =
      let dep_info = Model.dep_info dep in
      match RoleMap.find_opt dep_info.Model.dep_role report with
      | None -> None (* Not in the selections => can't be part of a conflict *)
      | Some required_component ->
        (match Component.selected_impl required_component with
         | None -> None (* Dummy selection can't cause a conflict *)
         | Some dep_impl ->
           let check_restriction r =
             if Model.meets_restriction dep_impl r
             then None
             else Some (`DepFailsRestriction (dep, r))
           in
           List.find_map ~f:check_restriction (Model.restrictions dep))
    in
    let deps = Model.requires role impl in
    List.find_map ~f:check_dep deps
  ;;

  (** A selected component has [dep] as a dependency. Use this to explain why some implementations
      of the required interface were rejected. *)
  let examine_dep requiring_role requiring_impl report dep =
    let { Model.dep_role = other_role; dep_importance = _ } = Model.dep_info dep in
    match RoleMap.find_opt other_role report with
    | None -> ()
    | Some required_component ->
      let dep_restrictions = Model.restrictions dep in
      if dep_restrictions <> []
      then
        (* Remove implementations incompatible with the other selections *)
        Component.apply_restrictions
          required_component
          dep_restrictions
          ~note:(Restricts (requiring_role, requiring_impl, dep_restrictions))
  ;;

  (* Find all restrictions that are in play and affect this interface *)
  let examine_selection report role component =
    (* Note any conflicts caused by <replaced-by> elements *)
    let () =
      match Component.replacement component with
      | Some replacement when RoleMap.mem replacement report ->
        Component.note component (ReplacedByConflict replacement);
        Component.reject_all component (`ConflictsRole replacement);
        (match RoleMap.find_opt replacement report with
         | Some replacement_component ->
           Component.note replacement_component (ReplacesConflict role);
           Component.reject_all replacement_component (`ConflictsRole role)
         | None -> ())
      | _ -> ()
    in
    match Component.selected_impl component with
    | Some our_impl ->
      (* For each dependency of our selected impl, explain why it rejected impls in the dependency's interface. *)
      let deps = Model.requires role our_impl in
      List.iter ~f:(examine_dep role our_impl report) deps
    | None ->
      (* For each of our remaining unrejected impls, check whether a dependency prevented its selection. *)
      Component.filter_impls component (get_dependency_problem role report)
  ;;

  (* Check for user-supplied restrictions *)
  let examine_extra_restrictions report =
    report
    |> RoleMap.iter (fun role component ->
      Model.user_restrictions role
      |> Option.iter (fun restriction ->
        Component.apply_user_restriction component restriction))
  ;;

  module Classes = Map.Make (struct
      type t = Model.conflict_class

      let compare = compare
    end)

  (** For each selected implementation with a conflict class, reject all candidates
      with the same class. *)
  let check_conflict_classes report =
    let classes =
      RoleMap.fold
        (fun role component acc ->
          match Component.selected_impl component with
          | None -> acc
          | Some impl ->
            Model.conflict_class impl
            |> List.fold_left ~f:(fun acc x -> Classes.add x role acc) ~init:acc)
        report
        Classes.empty
    in
    report
    |> RoleMap.iter
       @@ fun role component ->
       Component.filter_impls component
       @@ fun impl ->
       let rec aux = function
         | [] -> None
         | cl :: cls ->
           (match Classes.find_opt cl classes with
            | Some other_role when Model.Role.compare role other_role <> 0 ->
              Some (`ClassConflict (other_role, cl))
            | _ -> aux cls)
       in
       aux (Model.conflict_class impl)
  ;;

  let of_result result =
    let impls = Results.to_map result in
    let+ report =
      let get_selected role sel =
        let impl = Results.unwrap sel in
        let diagnostics = lazy (Results.explain result role) in
        let impl = if impl == Model.dummy_impl then None else Some impl in
        let* impl_candidates = Model.implementations role in
        let+ rejects, feed_problems = Model.rejects role in
        Component.create ~role (impl_candidates, rejects, feed_problems) diagnostics impl
      in
      RoleMap.bindings impls
      |> Fiber.parallel_map ~f:(fun (k, v) ->
        let+ v = get_selected k v in
        k, v)
      |> Fiber.map ~f:(fun s -> RoleMap.of_seq (List.to_seq s))
    in
    examine_extra_restrictions report;
    check_conflict_classes report;
    RoleMap.iter (examine_selection report) report;
    RoleMap.iter (fun _ c -> Component.finalise c) report;
    report
  ;;

  let pp_rolemap ~verbose reasons =
    let pp_item (_, c) = Pp.text "- " ++ Pp.box (Component.pp ~verbose c) in
    Pp.concat_map ~sep:Pp.cut (RoleMap.bindings reasons) ~f:pp_item
  ;;

  (** Return a message explaining why the solve failed. *)
  let get_failure_reason ?(verbose = false) result =
    let+ reasons = of_result result in
    Pp.paragraph "Can't find all required implementations:"
    ++ Pp.cut
    ++ Pp.vbox (pp_rolemap ~verbose reasons)
  ;;
end
