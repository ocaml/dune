open Import
module Non_evaluated_rule = Rule
module Anon_rule = Rule.Anonymous_action_rule
open Memo.O

module Rule = struct
  type t =
    { id : Rule.Id.t
    ; deps : Dep.Set.t
    ; expanded_deps : Path.Set.t
    ; targets : Targets.Validated.t option
    ; action : Action.t
    ; aliases : Alias_name.t list option
    ; loc : Loc.t
    }
end

module Rule_top_closure = Top_closure.Make (Non_evaluated_rule.Id.Set) (Memo)

module rec Expand : sig
  type expanded := Path.Set.t * Anon_rule.Set.t

  val alias : Alias.t -> expanded Memo.t
  val deps : Dep.Set.t -> expanded Memo.t
end = struct
  let combine_both fa fb (a1, b1) (a2, b2) = fa a1 a2, fb b1 b2

  let alias =
    let memo =
      Memo.create
        "expand-alias"
        ~input:(module Alias)
        (fun alias ->
           let* deps, anons =
             Load_rules.get_alias_definition alias
             >>= Memo.map_reduce
                   ~empty:(Dep.Set.empty, Anon_rule.Set.empty)
                   ~combine:(combine_both Dep.Set.union Anon_rule.Set.union)
                   ~f:(fun (loc, definition) ->
                     Memo.push_stack_frame
                       (fun () ->
                          match (definition : Rules.Dir_rules.Alias_spec.item) with
                          | Deps x ->
                            let+ (), deps = Action_builder.evaluate_and_collect_deps x in
                            deps, Anon_rule.Set.empty
                          | Action x ->
                            Memo.return (Dep.Set.empty, Anon_rule.Set.singleton x))
                       ~human_readable_description:(fun () -> Alias.describe alias ~loc))
           in
           let+ expanded_deps, anons' = Expand.deps deps in
           expanded_deps, Anon_rule.Set.union anons anons')
    in
    Memo.exec memo
  ;;

  let deps deps =
    Memo.map_reduce
      (Dep.Set.to_list deps)
      ~empty:(Path.Set.empty, Anon_rule.Set.empty)
      ~combine:(combine_both Path.Set.union Anon_rule.Set.union)
      ~f:(fun (dep : Dep.t) ->
        match dep with
        | File p -> Memo.return (Path.Set.singleton p, Anon_rule.Set.empty)
        | File_selector g ->
          let+ filenames = Build_system.eval_pred g in
          (* Alas, we can't use filename sets here because we end up putting paths coming
           from different directories together. *)
          Path.Set.of_list (Filename_set.to_list filenames), Anon_rule.Set.empty
        | Alias a -> Expand.alias a
        | Env _ | Universe -> Memo.return (Path.Set.empty, Anon_rule.Set.empty))
  ;;
end

let evaluate_rule =
  let memo =
    Memo.create
      "evaluate-rule"
      ~input:(module Non_evaluated_rule)
      (fun rule ->
         let* action, deps = Action_builder.evaluate_and_collect_deps rule.action in
         let* expanded_deps, _ = Expand.deps deps in
         Memo.return
           { Rule.id = rule.id
           ; deps
           ; expanded_deps
           ; targets = Some rule.targets
           ; action = action.action
           ; aliases = None
           ; loc = rule.loc
           })
  in
  Memo.exec memo
;;

let evaluate_anonymous_action =
  let memo =
    Memo.create
      "evaluate-anonymous-action"
      ~input:(module Non_evaluated_rule.Anonymous_action_rule)
      (fun anon_action ->
         let* action, deps =
           Action_builder.evaluate_and_collect_deps anon_action.action
         in
         let* expanded_deps, _ = Expand.deps deps in
         Memo.return
           { Rule.id = anon_action.id
           ; deps
           ; expanded_deps
           ; targets = None
           ; action = action.action
           ; aliases =
               (match anon_action.aliases with
                | [] -> None
                | aliases -> Some aliases)
           ; loc = anon_action.loc
           })
  in
  Memo.exec memo
;;

let rules_of_dep_paths paths =
  Path.Set.to_list paths
  |> Memo.parallel_map ~f:(fun p ->
    Load_rules.get_rule p
    >>= function
    | None -> Memo.return None
    | Some rule -> evaluate_rule rule >>| Option.some)
  >>| List.filter_opt
;;

let rules_of_anon_actions anons =
  Anon_rule.Set.to_list anons |> Memo.parallel_map ~f:evaluate_anonymous_action
;;

let rules_of_deps deps =
  let* dep_paths, anons = Expand.deps deps in
  let+ dep_rules, anon_rules =
    Memo.fork_and_join
      (fun () -> rules_of_dep_paths dep_paths)
      (fun () -> rules_of_anon_actions anons)
  in
  dep_rules @ anon_rules
;;

let eval ~recursive ~request =
  let* (), deps = Action_builder.evaluate_and_collect_deps request in
  let* root_rules = rules_of_deps deps in
  Rule_top_closure.top_closure
    root_rules
    ~key:(fun rule -> rule.Rule.id)
    ~deps:(fun rule -> if recursive then rules_of_deps rule.deps else Memo.return [])
  >>| function
  | Ok l -> l
  | Error cycle ->
    User_error.raise
      [ Pp.text "Dependency cycle detected:"
      ; Pp.chain cycle ~f:(function
          | { targets = Some targets; _ } ->
            Pp.verbatim
              (Path.to_string_maybe_quoted (Path.build (Targets.Validated.head targets)))
          | _ -> assert false)
      ]
;;
